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

#include <stdio.h>
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

/* NCRED & NDEBT handle minus the usual quantities */
#define NCRED_CELL     14
#define NDEBT_CELL     15

/* MXFRM is the "mirrored" transfer-from account */
#define MXFRM_CELL     16


#define DATE_CELL_WIDTH    11
#define NUM_CELL_WIDTH      7
#define ACTN_CELL_WIDTH     7
#define XFRM_CELL_WIDTH    14
#define MXFRM_CELL_WIDTH   14
#define XTO_CELL_WIDTH     14
#define DESC_CELL_WIDTH    29
#define MEMO_CELL_WIDTH    29
#define RECN_CELL_WIDTH     1
#define DEBT_CELL_WIDTH    12
#define CRED_CELL_WIDTH    12
#define NDEBT_CELL_WIDTH   12
#define NCRED_CELL_WIDTH   12
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
/* configLabels merely puts strings into the label cells 
 * it does *not* copy them to the header cursor */

static void
configLabels (SplitRegister *reg)
{
   BasicCell *hc;
   int type = (reg->type) & REG_TYPE_MASK;

   LABEL (DATE,  DATE_STR);
   LABEL (NUM,   NUM_STR);
   LABEL (ACTN,  NUM_STR);
   LABEL (XFRM,  XFRM_STR);
   LABEL (MXFRM, XFRM_STR);
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

   /* copy debit, dredit strings to ndebit, ncredit cells */
   hc = reg->header_label_cells[DEBT_CELL];
   LABEL (NDEBT,  hc->value);
   hc = reg->header_label_cells[CRED_CELL];
   LABEL (NCRED,  hc->value);
}

/* ============================================== */

#define SET(NAME,col,row,handler)				\
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

#define BASIC(NAME,CN,col,row) {			\
   SET (NAME, col, row, reg->CN##Cell);			\
}

#define FANCY(NAME,CN,col,row) {			\
   SET (NAME, col, row, &(reg->CN##Cell->cell));	\
}

/* ============================================== */

static void
configLayout (SplitRegister *reg)
{
   CellBlock *curs, *header;
   int type = (reg->type) & REG_TYPE_MASK;
   // int style = (reg->type) & REG_STYLE_MASK;
   int i;

   /* define header for macros */
   header = reg->header;

   /* fill things up with null cells */
   for (i=0; i<reg->num_cols; i++) {
      header->cells[0][i] = reg->nullCell;
      reg->split_cursor->cells[0][i] = reg->nullCell;
      reg->trans_cursor->cells[0][i] = reg->nullCell;
      reg->single_cursor->cells[0][i] = reg->nullCell;
      reg->double_cursor->cells[0][i] = reg->nullCell;
      reg->double_cursor->cells[1][i] = reg->nullCell;
   }

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
      {

         /* basic common 8 column config */
         curs = reg->single_cursor;
         FANCY (DATE,   date,     0,  0);
         BASIC (NUM,    num,      1,  0);
         FANCY (MXFRM,  mxfrm,    2,  0);
         FANCY (DESC,   desc,     3,  0);
         BASIC (RECN,   recn,     4,  0);
         FANCY (DEBT,   debit,    5,  0);
         FANCY (CRED,   credit,   6,  0);
         FANCY (BALN,   balance,  7,  0);

         curs = reg->double_cursor;
         FANCY (DATE,   date,     0,  0);
         BASIC (NUM,    num,      1,  0);
         FANCY (DESC,   desc,     3,  0);
         BASIC (RECN,   recn,     4,  0);
         FANCY (DEBT,   debit,    5,  0);
         FANCY (CRED,   credit,   6,  0);
         FANCY (BALN,   balance,  7,  0);
  
         FANCY (ACTN,   action,   1,  1);
         FANCY (MXFRM,  mxfrm,    2,  1);
         BASIC (MEMO,   memo,     3,  1);

         curs = reg->trans_cursor;
         FANCY (DATE,   date,     0,  0);
         BASIC (NUM,    num,      1,  0);
         FANCY (DESC,   desc,     3,  0);
         BASIC (RECN,   recn,     4,  0);
         FANCY (BALN,   balance,  7,  0);
     
         curs = reg->split_cursor;
         FANCY (ACTN,   action,   1,  0);
         FANCY (XFRM,   xfrm,     2,  0);
         BASIC (MEMO,   memo,     3,  0);
         FANCY (NDEBT,  ndebit,   5,  0);
         FANCY (NCRED,  ncredit,  6,  0);
         break;
      }

      /* --------------------------------------------------------- */
      case STOCK_REGISTER:
      case PORTFOLIO:
      {
         /* 11 column config */
         curs = reg->single_cursor;
         FANCY (DATE,   date,     0,  0);
         BASIC (NUM,    num,      1,  0);
         FANCY (MXFRM,  mxfrm,    2,  0);
         FANCY (DESC,   desc,     3,  0);
         BASIC (RECN,   recn,     4,  0);
         FANCY (DEBT,   debit,    5,  0);
         FANCY (CRED,   credit,   6,  0);
         FANCY (PRIC,   price,    7,  0);
         FANCY (VALU,   value,    8,  0);
         FANCY (SHRS,   shrs,     9,  0);
         FANCY (BALN,   balance,  10, 0);

         /* prep the second row of the double style */
         curs = reg->double_cursor;
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

         FANCY (ACTN,   action,   1,  1);
         FANCY (MXFRM,  mxfrm,    2,  1);
         BASIC (MEMO,   memo,     3,  1);

         /* only the transaction cursor gets used */
         curs = reg->trans_cursor;
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
    
         curs = reg->split_cursor;
         FANCY (ACTN,   action,   1,  0);
         FANCY (XFRM,   xfrm,     2,  0);
         BASIC (MEMO,   memo,     3,  0);
         FANCY (NDEBT,  ndebit,   5,  0);
         FANCY (NCRED,  ncredit,  6,  0);
         break;
      }
      /* --------------------------------------------------------- */
      default:
         printf ("Internal Error: configLayout(): "
           "unknown register type %d \n", type);
         break;
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

#define TRAVERSE_NON_NULL_CELLS() {			\
   i = prev_r;						\
   for (j=prev_c+1; j<curs->numCols; j++) {		\
      if ((reg->nullCell != curs->cells[i][j]) &&	\
          (reg->recnCell != curs->cells[i][j]) &&	\
          (XACC_CELL_ALLOW_INPUT & curs->cells[i][j]->input_output)) \
      {							\
         NEXT_RIGHT  (i, j);				\
      }							\
   }							\
   for (i=prev_r+1; i<curs->numRows; i++) {		\
      for (j=0; j<curs->numCols; j++) {			\
         if ((reg->nullCell != curs->cells[i][j]) &&	\
             (reg->recnCell != curs->cells[i][j]) &&	\
             (XACC_CELL_ALLOW_INPUT & curs->cells[i][j]->input_output)) \
         {						\
            NEXT_RIGHT  (i, j);				\
         }						\
      }							\
   }							\
}

#define FIRST_NON_NULL(r,c) {				\
   i = r;						\
   for (j=c; j<curs->numCols; j++) {			\
      if ((reg->nullCell != curs->cells[i][j]) &&	\
          (reg->recnCell != curs->cells[i][j]) &&	\
          (XACC_CELL_ALLOW_INPUT & curs->cells[i][j]->input_output)) \
      {							\
         FIRST_RIGHT  (i, j);				\
         break;						\
      }							\
   }							\
}

#define NEXT_NON_NULL(r,c) {				\
   i = r;						\
   for (j=c+1; j<curs->numCols; j++) {			\
      if ((reg->nullCell != curs->cells[i][j]) &&	\
          (reg->recnCell != curs->cells[i][j]) &&	\
          (XACC_CELL_ALLOW_INPUT & curs->cells[i][j]->input_output)) \
      {							\
         NEXT_RIGHT  (i, j);				\
         break;						\
      }							\
   }							\
}

#define NEXT_SPLIT() {					\
   i = 0;						\
   for (j=0; j<reg->split_cursor->numCols; j++) {	\
      if ((reg->nullCell != reg->split_cursor->cells[i][j]) &&	\
          (reg->recnCell != reg->split_cursor->cells[i][j]) &&	\
          (XACC_CELL_ALLOW_INPUT & reg->split_cursor->cells[i][j]->input_output)) \
      {							\
         NEXT_RIGHT  (i+1, j);				\
         break;						\
      }							\
   }							\
}

static void
configTraverse (SplitRegister *reg)
{
   int i,j;
   int prev_r=0, prev_c=0;
   int first_r, first_c;
   CellBlock *curs = NULL;

   curs = reg->single_cursor;
   /* lead in with the date cell, return to the date cell */
   FIRST_NON_NULL (0, 0);
   first_r = prev_r; first_c = prev_c;
   TRAVERSE_NON_NULL_CELLS ();
   /* wrap back to start of row after hitting the commit button */
   NEXT_RIGHT (-1-first_r, -1-first_c);

   curs = reg->double_cursor;
   /* lead in with the date cell, return to the date cell */
   FIRST_NON_NULL (0, 0);
   first_r = prev_r; first_c = prev_c;
   TRAVERSE_NON_NULL_CELLS ();
   /* for double-line, hop back one row */
   NEXT_RIGHT (-1-first_r, -1-first_c);

   curs = reg->trans_cursor;
   FIRST_NON_NULL (0,0);
   TRAVERSE_NON_NULL_CELLS ();
   /* hop to start of next row (the split cursor) */
   NEXT_SPLIT();

   curs = reg->split_cursor;
   FIRST_NON_NULL (0,0);
   TRAVERSE_NON_NULL_CELLS ();
   /* hop to start of next row (the split cursor) */
   NEXT_SPLIT();
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
   /* --------------------------- */
   /* set the color of the cells in the cursors */
   /* hack alert -- the actual color should depend on the 
    * type of register. */
   reg->single_cursor->active_bg_color = 0xffdddd; /* pale red */
   reg->single_cursor->passive_bg_color = 0xccccff; /* pale blue */

   reg->double_cursor->active_bg_color = 0xffdddd; /* pale red */
   reg->double_cursor->passive_bg_color = 0xccccff; /* pale blue */

   reg->trans_cursor->active_bg_color = 0xffdddd; /* pale red */
   reg->trans_cursor->passive_bg_color = 0xccccff; /* pale blue */

   reg->split_cursor->active_bg_color = 0xffffdd; /* pale yellow */
   reg->split_cursor->passive_bg_color = 0xffffff; /* white */

}

/* ============================================== */

static void
mallocCursors (SplitRegister *reg)
{
   int type = (reg->type) & REG_TYPE_MASK;

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
         break;

      case STOCK_REGISTER:
      case PORTFOLIO:
         reg->num_cols = 11;
         break;
      default:
         break;
   }

   reg->num_header_rows = 1;
   reg->header = xaccMallocCellBlock (reg->num_header_rows, reg->num_cols);

   /* cursors used in the single & double line displays */
   reg->single_cursor = xaccMallocCellBlock (1, reg->num_cols);
   reg->double_cursor = xaccMallocCellBlock (2, reg->num_cols);

   /* the two cursors used for multi-line and dynamic displays */
   reg->trans_cursor = xaccMallocCellBlock (1, reg->num_cols);
   reg->split_cursor = xaccMallocCellBlock (1, reg->num_cols);

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
   reg->user_hack = NULL;
   reg->destroy = NULL;
   reg->type = type;

   /* --------------------------- */
   /* define the number of columns in the display, malloc the cursors */
   mallocCursors (reg);

   /* --------------------------- */
   /* malloc the header (label) cells */
   header = reg->header;

   HDR (DATE);
   HDR (NUM);
   HDR (ACTN);
   HDR (XFRM);
   HDR (MXFRM);
   HDR (XTO);
   HDR (DESC);
   HDR (MEMO);
   HDR (RECN);
   HDR (CRED);
   HDR (DEBT);
   HDR (PRIC);
   HDR (VALU);
   HDR (SHRS);
   HDR (BALN);
   
   HDR (NCRED);
   HDR (NDEBT);

   /* --------------------------- */
   /* malloc the workhorse cells */

   NEW (null,    Basic);
   NEW (date,    Date);
   NEW (num,     Text);
   NEW (desc,    QuickFill);
   NEW (recn,    Recn);
   NEW (shrs,    Price);
   NEW (balance, Price);

   NEW (xfrm,    Combo);
   NEW (mxfrm,   Combo);
   NEW (xto,     Combo);
   NEW (action,  Combo);
   NEW (memo,    Text);
   NEW (credit,  Price);
   NEW (debit,   Price);
   NEW (price,   Price);
   NEW (value,   Price);

   NEW (ncredit, Price);
   NEW (ndebit,  Price);

   /* --------------------------- */
   /* configLabels merely puts strings into the label cells 
    * it does *not* copy them to the header cursor */
   configLabels (reg);

   /* config the layout of the cells in the cursors */
   configLayout (reg);

   /* --------------------------- */
   /* do some misc cell config */
   configCursors (reg);

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
   reg->ndebitCell->blank_zero = 1;
   reg->ncreditCell->blank_zero = 1;

   /* ok, now make sure the initail value of 0.0 is blanked.
    * if this is not done, then various oddball situations 
    * will show the non-blanked values. 
    */
   xaccSetPriceCellValue (reg->debitCell, 0.0);
   xaccSetPriceCellValue (reg->creditCell, 0.0);
   xaccSetPriceCellValue (reg->valueCell, 0.0);
   xaccSetPriceCellValue (reg->ndebitCell, 0.0);
   xaccSetPriceCellValue (reg->ncreditCell, 0.0);

   /* use three decimal places to print share-related info.
    * The format is a printf-style format for a double.  */
   xaccSetPriceCellFormat (reg->shrsCell, "%.3f");
   xaccSetPriceCellFormat (reg->priceCell, "%.3f");

   /* -------------------------------- */   
   /* define how traversal works. This must be done *after* the balance, etc.
    * cells have been marked read-only, since otherwise config will try
    * to pick them up.
    */
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
   phys_r = header->numRows;
   reg->cursor_phys_row = phys_r;  /* cursor on first line past header */
   reg->cursor_virt_row = 1;

   phys_r += reg->single_cursor->numRows;
   reg->num_phys_rows = phys_r;
   reg->num_virt_rows = 2;  /* one header, one single_cursor */

   phys_c = header->numCols;
   reg->num_cols = phys_c;

   table = xaccMallocTable ();
   xaccSetTableSize (table, phys_r, phys_c, reg->num_virt_rows, 1);
   xaccSetCursor (table, header, 0, 0, 0, 0);

   /* the SetCursor call below is for most practical purposes useless.
    * It simply installs a cursor (the single-line cursor, but it could 
    * of been any of them), and moves it to the first editable row.
    * Whoop-de-doo, since this is promptly over-ridden when real data 
    * gets loaded.  Its just sort of here as a fail-safe fallback,
    * in case someone just creates a register but doesn't do anything 
    * with it.  Don't want to freak out any programmers.
    */
   xaccSetCursor (table, reg->single_cursor, 
                         reg->cursor_phys_row, 0, 
                         reg->cursor_virt_row, 0);
   xaccMoveCursor (table, header->numRows, 0);

   reg->table = table;
}

/* ============================================== */

void
xaccConfigSplitRegister (SplitRegister *reg, int newtype)
{
   if (!reg) return;

   reg->type = newtype;

   /* Make sure that any GU elemnts associated with this reconfig 
    * are properly initialized.  */
   xaccCreateCursor (reg->table, reg->single_cursor);
   xaccCreateCursor (reg->table, reg->double_cursor);
   xaccCreateCursor (reg->table, reg->trans_cursor);
   xaccCreateCursor (reg->table, reg->split_cursor);
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
   reg->user_hack = NULL;

   xaccDestroyTable (reg->table);
   reg->table = NULL;

   xaccDestroyCellBlock (reg->header);
   xaccDestroyCellBlock (reg->single_cursor);
   xaccDestroyCellBlock (reg->double_cursor);
   xaccDestroyCellBlock (reg->trans_cursor);
   xaccDestroyCellBlock (reg->split_cursor);
   reg->header = NULL;
   reg->single_cursor = NULL;
   reg->double_cursor = NULL;
   reg->trans_cursor = NULL;
   reg->split_cursor = NULL;

   xaccDestroyDateCell      (reg->dateCell);
   xaccDestroyBasicCell     (reg->numCell);
   xaccDestroyQuickFillCell (reg->descCell);
   xaccDestroyBasicCell     (reg->recnCell);
   xaccDestroyPriceCell     (reg->shrsCell);
   xaccDestroyPriceCell     (reg->balanceCell);

   xaccDestroyComboCell     (reg->actionCell);
   xaccDestroyComboCell     (reg->xfrmCell);
   xaccDestroyComboCell     (reg->mxfrmCell);
   xaccDestroyComboCell     (reg->xtoCell);
   xaccDestroyBasicCell     (reg->memoCell);
   xaccDestroyPriceCell     (reg->creditCell);
   xaccDestroyPriceCell     (reg->debitCell);
   xaccDestroyPriceCell     (reg->priceCell);
   xaccDestroyPriceCell     (reg->valueCell);

   xaccDestroyPriceCell     (reg->ncreditCell);
   xaccDestroyPriceCell     (reg->ndebitCell);

   reg->dateCell    = NULL;
   reg->numCell     = NULL;
   reg->descCell    = NULL;
   reg->recnCell    = NULL;
   reg->shrsCell    = NULL;
   reg->balanceCell = NULL;

   reg->actionCell  = NULL;
   reg->xfrmCell    = NULL;
   reg->mxfrmCell   = NULL;
   reg->xtoCell     = NULL;
   reg->memoCell    = NULL;
   reg->creditCell  = NULL;
   reg->debitCell   = NULL;
   reg->priceCell   = NULL;
   reg->valueCell   = NULL;

   reg->ncreditCell  = NULL;
   reg->ndebitCell   = NULL;

   /* free the memory itself */
   free (reg);
}

/* ============================================== */

unsigned int
xaccSplitRegisterGetChangeFlag (SplitRegister *reg)
{

   unsigned int changed = 0;

   /* be careful to use bitwise ands and ors to assemble bit flag */
   changed |= MOD_DATE  & reg->dateCell->cell.changed;
   changed |= MOD_NUM   & reg->numCell->changed;
   changed |= MOD_DESC  & reg->descCell->cell.changed;
   changed |= MOD_RECN  & reg->recnCell->changed;

   changed |= MOD_ACTN  & reg->actionCell->cell.changed;
   changed |= MOD_XFRM  & reg->xfrmCell->cell.changed;
   changed |= MOD_MXFRM & reg->mxfrmCell->cell.changed;
   changed |= MOD_XTO   & reg->xtoCell->cell.changed; 
   changed |= MOD_MEMO  & reg->memoCell->changed;
   changed |= MOD_AMNT  & reg->creditCell->cell.changed;
   changed |= MOD_AMNT  & reg->debitCell->cell.changed;
   changed |= MOD_PRIC  & reg->priceCell->cell.changed;
   changed |= MOD_VALU  & reg->valueCell->cell.changed; 

   changed |= MOD_NAMNT & reg->ncreditCell->cell.changed;
   changed |= MOD_NAMNT & reg->ndebitCell->cell.changed;

   return changed;
}

/* ============ END OF FILE ===================== */
