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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

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
 * Copyright (c) 1998, 1999, 2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas <dave@krondo.com>
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <glib.h>

#include "recncell.h"
#include "splitreg.h"
#include "table-allgui.h"
#include "textcell.h"
#include "messages.h"
#include "util.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

static SRStringGetter debit_getter = NULL;
static SRStringGetter credit_getter = NULL;

typedef struct _CellBuffer CellBuffer;
struct _CellBuffer
{
  char * value;
  unsigned int changed;
};

struct _SplitRegisterBuffer
{
  CellBuffer dateCell;
  CellBuffer numCell;
  CellBuffer descCell;
  CellBuffer recnCell;
  CellBuffer shrbalnCell;
  CellBuffer balanceCell;
  CellBuffer actionCell;
  CellBuffer xfrmCell;
  CellBuffer mxfrmCell;
  CellBuffer xtoCell;
  CellBuffer memoCell;
  CellBuffer creditCell;
  CellBuffer debitCell;
  CellBuffer priceCell;
  CellBuffer sharesCell;
};

static SplitRegisterColors reg_colors = {
  0xffdddd, /* pale red, single cursor active */
  0xccccff, /* pale blue, single cursor passive */
  0xccccff, /* pale blue, single cursor passive 2 */

  0xffdddd, /* pale red, double cursor active */
  0xccccff, /* pale blue, double cursor passive */
  0xffffff, /* white, double cursor passive 2 */

  FALSE,    /* double mode alternate by physical row */

  0xffdddd, /* pale red, trans cursor active */
  0xccccff, /* pale blue, trans cursor passive */

  0xffffdd, /* pale yellow, split cursor active */
  0xffffff, /* white, split cursor passive */

  0xffffff  /* white, header color */
};

#define DATE_CELL_ALIGN    CELL_ALIGN_RIGHT
#define NUM_CELL_ALIGN     CELL_ALIGN_LEFT
#define ACTN_CELL_ALIGN    CELL_ALIGN_LEFT
#define XFRM_CELL_ALIGN    CELL_ALIGN_RIGHT
#define MXFRM_CELL_ALIGN   CELL_ALIGN_RIGHT
#define XTO_CELL_ALIGN     CELL_ALIGN_RIGHT
#define DESC_CELL_ALIGN    CELL_ALIGN_LEFT
#define MEMO_CELL_ALIGN    CELL_ALIGN_LEFT
#define RECN_CELL_ALIGN    CELL_ALIGN_CENTER
#define DEBT_CELL_ALIGN    CELL_ALIGN_RIGHT
#define CRED_CELL_ALIGN    CELL_ALIGN_RIGHT
#define PRIC_CELL_ALIGN    CELL_ALIGN_RIGHT
#define SHRS_CELL_ALIGN    CELL_ALIGN_RIGHT
#define SHRBALN_CELL_ALIGN CELL_ALIGN_RIGHT
#define BALN_CELL_ALIGN    CELL_ALIGN_RIGHT


static void
xaccInitSplitRegister (SplitRegister *reg,
                       SplitRegisterType type,
                       SplitRegisterStyle style,
                       TableGetEntryHandler entry_handler,
                       VirtCellDataAllocator allocator,
                       VirtCellDataDeallocator deallocator,
                       VirtCellDataCopy copy);


/* ============================================== */

#define LABEL(NAME,label)					\
{								\
   BasicCell *hcell;						\
   hcell = reg->header_label_cells[NAME##_CELL];		\
   xaccSetBasicCellValue (hcell, label);			\
}

/* ============================================== */

void
xaccSplitRegisterSetDebitStringGetter(SRStringGetter getter)
{
  debit_getter = getter;
}

void
xaccSplitRegisterSetCreditStringGetter(SRStringGetter getter)
{
  credit_getter = getter;
}

/* ============================================== */
/* configLabels merely puts strings into the label cells 
 * it does *not* copy them to the header cursor */

static void
configLabels (SplitRegister *reg)
{
  SplitRegisterType type;
  char *string;

  type = reg->type;

  LABEL (DATE,    DATE_STR);
  LABEL (NUM,     NUM_STR);
  LABEL (DESC,    DESC_STR);
  LABEL (RECN,    RECONCILE_ABBREV);
  LABEL (SHRBALN, TOTAL_SHARES_STR);
  LABEL (BALN,    BALN_STR);
  LABEL (ACTN,    ACTION_STR);
  LABEL (XFRM,    ACCOUNT_STR);
  LABEL (MXFRM,   TRANSFER_STR);
  LABEL (XTO,     ACCOUNT_STR);
  LABEL (MEMO,    MEMO_STR);
  LABEL (CRED,    CREDIT_STR);
  LABEL (DEBT,    DEBIT_STR);
  LABEL (PRIC,    PRICE_STR);
  LABEL (SHRS,    SHARES_STR);

  if (debit_getter != NULL)
  {
    string = debit_getter(type);
    if (string != NULL)
    {
      LABEL (DEBT, string);
      free(string);
    }
  }

  if (credit_getter != NULL)
  {
    string = credit_getter(type);
    if (string != NULL)
    {
      LABEL (CRED, string);
      free(string);
    }
  }
}

/* ============================================== */
/* configAction strings into the action cell */
/* hack alert -- this stuff really, really should be in a config file ... */

static void
configAction (SplitRegister *reg)
{
  /* setup strings in the action pull-down */
  switch (reg->type)
  {
    case BANK_REGISTER:
    case SEARCH_LEDGER:  /* broken ! FIXME bg */
      xaccAddComboCellMenuItem ( reg->actionCell, DEPOSIT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, WITHDRAW_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, CHECK_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, INT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, ATM_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, TELLER_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, POS_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, ARU_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, ONLINE_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, ACH_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, WIRE_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, CREDIT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, DIRECTDEBIT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, TRANSFER_STR);
      break;
    case CASH_REGISTER:
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      break;
    case ASSET_REGISTER:
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, FEE_STR);
      break;
    case CREDIT_REGISTER:
      xaccAddComboCellMenuItem ( reg->actionCell, ATM_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, CREDIT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, FEE_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, INT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, ONLINE_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      break;
    case LIABILITY_REGISTER:
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, LOAN_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, INT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, PAYMENT_STR);
      break;
    case INCOME_LEDGER:
    case INCOME_REGISTER:
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, INT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, PAYMENT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, REBATE_STR);
      break;
    case EXPENSE_REGISTER:
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      break;
    case GENERAL_LEDGER:
    case EQUITY_REGISTER:
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, EQUITY_STR);
      break;
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
    case CURRENCY_REGISTER:
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, PRICE_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, FEE_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, DIV_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, INT_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, LTCG_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, STCG_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, INCOME_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, DIST_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SPLIT_STR);
      break;
    default:
      xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
      xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
      break;
  }
}

/* ============================================== */

#define SET(NAME,col,row,handler)			      \
{							      \
   BasicCell *hcell;					      \
   hcell = reg->header_label_cells[NAME##_CELL];	      \
							      \
   if ((0<=row) && (0<=col)) {				      \
      CellBlockCell *cb_cell;                                 \
                                                              \
      cb_cell = gnc_cellblock_get_cell (curs, row, col);      \
                                                              \
      cb_cell->cell = (handler);			      \
      cb_cell->cell_type = NAME##_CELL;                       \
      cb_cell->sample_text = g_strdup (NAME##_CELL_SAMPLE);   \
      cb_cell->alignment = NAME##_CELL_ALIGN;		      \
      cb_cell->expandable = ((handler) == (BasicCell *) reg->descCell);     \
      cb_cell->span = ((handler) == (BasicCell *) reg->memoCell);     \
                                                              \
      cb_cell = gnc_cellblock_get_cell (header, row, col);    \
      if (cb_cell && (curs == reg->single_cursor)) {          \
        cb_cell->cell = hcell;			              \
        cb_cell->cell_type = NAME##_CELL;                     \
        cb_cell->sample_text = g_strdup (NAME##_CELL_SAMPLE); \
        cb_cell->alignment = NAME##_CELL_ALIGN;		      \
        cb_cell->expandable = ((handler) == (BasicCell *) reg->descCell);   \
        cb_cell->span = ((handler) == (BasicCell *) reg->memoCell);   \
      }							      \
   }                                                          \
}

/* SET_CELL macro initializes cells in the register */

#define SET_CELL(NAME,CN,col,row) {		\
  SET (NAME, col, row, &(reg->CN##Cell->cell));	\
}

/* ============================================== */

static void
configLayout (SplitRegister *reg)
{
  CellBlock *curs, *header;
  int i;

  /* define header for macros */
  header = reg->header;

  /* fill things up with null cells */
  for (i=0; i < header->num_cols; i++)
  {
    CellBlockCell *cb_cell;

    cb_cell = gnc_cellblock_get_cell (header, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->split_cursor, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->trans_cursor, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->single_cursor, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->double_cursor, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->double_cursor, 1, i);
    cb_cell->cell = reg->nullCell;
  }

  switch (reg->type)
  {
    case BANK_REGISTER:
    case CASH_REGISTER:
    case ASSET_REGISTER:
    case CREDIT_REGISTER:
    case LIABILITY_REGISTER:
    case INCOME_REGISTER:
    case EXPENSE_REGISTER:
    case EQUITY_REGISTER:
      {
        curs = reg->double_cursor;
        SET_CELL (DATE,   date,     0,  0);
        SET_CELL (NUM,    num,      1,  0);
        SET_CELL (DESC,   desc,     2,  0);
        SET_CELL (MXFRM,  mxfrm,    3,  0);
        SET_CELL (RECN,   recn,     4,  0);
        SET_CELL (DEBT,   debit,    5,  0);
        SET_CELL (CRED,   credit,   6,  0);
        SET_CELL (BALN,   balance,  7,  0);

        SET_CELL (ACTN,   action,   1,  1);
        SET_CELL (MEMO,   memo,     2,  1);

        curs = reg->trans_cursor;
        SET_CELL (DATE,   date,     0,  0);
        SET_CELL (NUM,    num,      1,  0);
        SET_CELL (DESC,   desc,     2,  0);
        SET_CELL (XTO,    xto,      3,  0);
        SET_CELL (RECN,   recn,     4,  0);
        SET_CELL (DEBT,   debit,    5,  0);
        SET_CELL (CRED,   credit,   6,  0);
        SET_CELL (BALN,   balance,  7,  0);

        curs = reg->split_cursor;
        SET_CELL (ACTN,   action,   1,  0);
        SET_CELL (MEMO,   memo,     2,  0);
        SET_CELL (XFRM,   xfrm,     3,  0);
        SET_CELL (CRED,   credit,   5,  0);
        SET_CELL (DEBT,   debit,    6,  0);

        curs = reg->single_cursor;
        SET_CELL (DATE,   date,     0,  0);
        SET_CELL (NUM,    num,      1,  0);
        SET_CELL (DESC,   desc,     2,  0);
        SET_CELL (MXFRM,  mxfrm,    3,  0);
        SET_CELL (RECN,   recn,     4,  0);
        SET_CELL (DEBT,   debit,    5,  0);
        SET_CELL (CRED,   credit,   6,  0);
        SET_CELL (BALN,   balance,  7,  0);

        break;
      }

      /* --------------------------------------------------------- */
    case INCOME_LEDGER:
    case GENERAL_LEDGER:
    case SEARCH_LEDGER:
      {
        curs = reg->double_cursor;
        SET_CELL (DATE,   date,     0,  0);
        SET_CELL (NUM,    num,      1,  0);
        SET_CELL (DESC,   desc,     2,  0);
        SET_CELL (XTO,    xto,      3,  0);
        SET_CELL (MXFRM,  mxfrm,    4,  0);
        SET_CELL (RECN,   recn,     5,  0);
        SET_CELL (DEBT,   debit,    6,  0);
        SET_CELL (CRED,   credit,   7,  0);

        SET_CELL (ACTN,   action,   1,  1);
        SET_CELL (MEMO,   memo,     2,  1);

        curs = reg->trans_cursor;
        SET_CELL (DATE,   date,     0,  0);
        SET_CELL (NUM,    num,      1,  0);
        SET_CELL (DESC,   desc,     2,  0);
        SET_CELL (XTO,    mxfrm,    3,  0);
        SET_CELL (XFRM,   xto,      4,  0);
        SET_CELL (RECN,   recn,     5,  0);
        SET_CELL (DEBT,   debit,    6,  0);
        SET_CELL (CRED,   credit,   7,  0);

        curs = reg->split_cursor;
        SET_CELL (ACTN,   action,   1,  0);
        SET_CELL (MEMO,   memo,     2,  0);
        SET_CELL (XFRM,   xfrm,     4,  0);
        SET_CELL (CRED,   credit,   6,  0);
        SET_CELL (DEBT,   debit,    7,  0);

        curs = reg->single_cursor;
        SET_CELL (DATE,   date,     0,  0);
        SET_CELL (NUM,    num,      1,  0);
        SET_CELL (DESC,   desc,     2,  0);
        SET_CELL (XTO,    xto,      3,  0);
        SET_CELL (MXFRM,  mxfrm,    4,  0);
        SET_CELL (RECN,   recn,     5,  0);
        SET_CELL (DEBT,   debit,    6,  0);
        SET_CELL (CRED,   credit,   7,  0);

        break;
      }

      /* --------------------------------------------------------- */
    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      {
        curs = reg->double_cursor;
        SET_CELL (DATE,    date,     0,  0);
        SET_CELL (NUM,     num,      1,  0);
        SET_CELL (DESC,    desc,     2,  0);
        SET_CELL (MXFRM,   mxfrm,    3,  0);
        SET_CELL (RECN,    recn,     4,  0);
        SET_CELL (SHRS,    shares,   5,  0);
        SET_CELL (PRIC,    price,    6,  0);
        SET_CELL (DEBT,    debit,    7,  0);
        SET_CELL (CRED,    credit,   8,  0);
        SET_CELL (SHRBALN, shrbaln,  9,  0);
        SET_CELL (BALN,    balance, 10,  0);

        SET_CELL (ACTN,    action,   1,  1);
        SET_CELL (MEMO,    memo,     2,  1);

        curs = reg->trans_cursor;
        SET_CELL (DATE,    date,     0,  0);
        SET_CELL (NUM,     num,      1,  0);
        SET_CELL (DESC,    desc,     2,  0);
        SET_CELL (XTO,     xto,      3,  0);
        SET_CELL (RECN,    recn,     4,  0);
        SET_CELL (SHRS,    shares,   5,  0);
        SET_CELL (PRIC,    price,    6,  0);
        SET_CELL (DEBT,    debit,    7,  0);
        SET_CELL (CRED,    credit,   8,  0);
        SET_CELL (SHRBALN, shrbaln,  9,  0);
        SET_CELL (BALN,    balance,  10, 0);

        curs = reg->split_cursor;
        SET_CELL (ACTN,    action,   1,  0);
        SET_CELL (MEMO,    memo,     2,  0);
        SET_CELL (XFRM,    xfrm,     3,  0);
        SET_CELL (CRED,    credit,   7,  0);
        SET_CELL (DEBT,    debit,    8,  0);

        curs = reg->single_cursor;
        SET_CELL (DATE,    date,     0,  0);
        SET_CELL (NUM,     num,      1,  0);
        SET_CELL (DESC,    desc,     2,  0);
        SET_CELL (MXFRM,   mxfrm,    3,  0);
        SET_CELL (RECN,    recn,     4,  0);
        SET_CELL (SHRS,    shares,   5,  0);
        SET_CELL (PRIC,    price,    6,  0);
        SET_CELL (DEBT,    debit,    7,  0);
        SET_CELL (CRED,    credit,   8,  0);
        SET_CELL (SHRBALN, shrbaln,  9,  0);
        SET_CELL (BALN,    balance, 10,  0);

        break;
      }

      /* --------------------------------------------------------- */
    case PORTFOLIO_LEDGER:
      {
        curs = reg->double_cursor;
        SET_CELL (DATE,    date,     0,  0);
        SET_CELL (NUM,     num,      1,  0);
        SET_CELL (DESC,    desc,     2,  0);
        SET_CELL (XTO,     xto,      3,  0);
        SET_CELL (MXFRM,   mxfrm,    4,  0);
        SET_CELL (RECN,    recn,     5,  0);
        SET_CELL (SHRS,    shares,   6,  0);
        SET_CELL (PRIC,    price,    7,  0);
        SET_CELL (DEBT,    debit,    8,  0);
        SET_CELL (CRED,    credit,   9,  0);
        SET_CELL (SHRBALN, shrbaln, 10,  0);

        SET_CELL (ACTN,    action,   1,  1);
        SET_CELL (MEMO,    memo,     2,  1);

        curs = reg->trans_cursor;
        SET_CELL (DATE,    date,     0,  0);
        SET_CELL (NUM,     num,      1,  0);
        SET_CELL (DESC,    desc,     2,  0);
        SET_CELL (XTO,     mxfrm,    3,  0);
        SET_CELL (XFRM,    xto,      4,  0);
        SET_CELL (RECN,    recn,     5,  0);
        SET_CELL (SHRS,    shares,   6,  0);
        SET_CELL (PRIC,    price,    7,  0);
        SET_CELL (DEBT,    debit,    8,  0);
        SET_CELL (CRED,    credit,   9,  0);
        SET_CELL (SHRBALN, shrbaln, 10,  0);

        curs = reg->split_cursor;
        SET_CELL (ACTN,    action,   1,  0);
        SET_CELL (MEMO,    memo,     2,  0);
        SET_CELL (XFRM,    xfrm,     4,  0);
        SET_CELL (CRED,    credit,   8,  0);
        SET_CELL (DEBT,    debit,    9,  0);

        curs = reg->single_cursor;
        SET_CELL (DATE,    date,     0,  0);
        SET_CELL (NUM,     num,      1,  0);
        SET_CELL (DESC,    desc,     2,  0);
        SET_CELL (XTO,     xto,      3,  0);
        SET_CELL (MXFRM,   mxfrm,    4,  0);
        SET_CELL (RECN,    recn,     5,  0);
        SET_CELL (SHRS,    shares,   6,  0);
        SET_CELL (PRIC,    price,    7,  0);
        SET_CELL (DEBT,    debit,    8,  0);
        SET_CELL (CRED,    credit,   9,  0);
        SET_CELL (SHRBALN, shrbaln, 10,  0);

        break;
      }

      /* --------------------------------------------------------- */
    default:
      PERR ("unknown register type %d \n", reg->type);
      break;
  }
}

/* ============================================== */

SplitRegister *
xaccMallocSplitRegister (SplitRegisterType type,
                         SplitRegisterStyle style,
                         TableGetEntryHandler entry_handler,
                         VirtCellDataAllocator allocator,
                         VirtCellDataDeallocator deallocator,
                         VirtCellDataCopy copy)
{
  SplitRegister * reg;

  reg = g_new(SplitRegister, 1);

  xaccInitSplitRegister (reg, type, style, entry_handler,
                         allocator, deallocator, copy);

  return reg;
}

/* ============================================== */

void
xaccSetSplitRegisterColors (SplitRegisterColors reg_colors_new)
{
  reg_colors = reg_colors_new;
}

/* ============================================== */

static void
configTable(SplitRegister *reg)
{
  if ((reg == NULL) || (reg->table == NULL))
    return;

  switch (reg->style) {
    case REG_SINGLE_LINE:
    case REG_SINGLE_DYNAMIC:
      reg->table->alternate_bg_colors = TRUE;
      break;
    case REG_DOUBLE_LINE:
    case REG_DOUBLE_DYNAMIC:
      reg->table->alternate_bg_colors = reg_colors.double_alternate_virt;
      break;
    default:
      reg->table->alternate_bg_colors = FALSE;
      break;
  }
}

/* ============================================== */

void
xaccSplitRegisterConfigColors (SplitRegister *reg)
{
  reg->single_cursor->active_bg_color =
    reg_colors.single_cursor_active_bg_color;
  reg->single_cursor->passive_bg_color =
    reg_colors.single_cursor_passive_bg_color;
  reg->single_cursor->passive_bg_color2 =
    reg_colors.single_cursor_passive_bg_color2;

  reg->double_cursor->active_bg_color =
    reg_colors.double_cursor_active_bg_color;
  reg->double_cursor->passive_bg_color =
    reg_colors.double_cursor_passive_bg_color;
  reg->double_cursor->passive_bg_color2 =
    reg_colors.double_cursor_passive_bg_color2;

  reg->trans_cursor->active_bg_color =
    reg_colors.trans_cursor_active_bg_color;
  reg->trans_cursor->passive_bg_color =
    reg_colors.trans_cursor_passive_bg_color;
  reg->trans_cursor->passive_bg_color2 =
    reg_colors.trans_cursor_passive_bg_color;

  reg->split_cursor->active_bg_color =
    reg_colors.split_cursor_active_bg_color;
  reg->split_cursor->passive_bg_color =
    reg_colors.split_cursor_passive_bg_color;
  reg->split_cursor->passive_bg_color2 =
    reg_colors.split_cursor_passive_bg_color;

  reg->header->active_bg_color = reg_colors.header_bg_color;
  reg->header->passive_bg_color = reg_colors.header_bg_color;
  reg->header->passive_bg_color2 = reg_colors.header_bg_color;

  configTable(reg);
}

/* ============================================== */

static void
configCursors (SplitRegister *reg)
{
  xaccSplitRegisterConfigColors(reg);
}

/* ============================================== */

static void
mallocCursors (SplitRegister *reg)
{
  int num_cols;

  switch (reg->type) {
    case BANK_REGISTER:
    case CASH_REGISTER:
    case ASSET_REGISTER:
    case CREDIT_REGISTER:
    case LIABILITY_REGISTER:
    case INCOME_REGISTER:
    case EXPENSE_REGISTER:
    case EQUITY_REGISTER:
      num_cols = 8;
      break;

    case INCOME_LEDGER:
    case GENERAL_LEDGER:
    case SEARCH_LEDGER:
      num_cols = 8;
      break;

    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      num_cols = 11;
      break;

    case PORTFOLIO_LEDGER:
      num_cols = 11;
      break;

    default:
      PERR("Bad register type");
      g_assert (FALSE);
      return;
  }

  reg->num_header_rows = 1;
  reg->header = gnc_cellblock_new (reg->num_header_rows, num_cols);

  /* cursors used in the single & double line displays */
  reg->single_cursor = gnc_cellblock_new (1, num_cols);
  reg->double_cursor = gnc_cellblock_new (2, num_cols);

  /* the two cursors used for multi-line and dynamic displays */
  reg->trans_cursor = gnc_cellblock_new (1, num_cols);
  reg->split_cursor = gnc_cellblock_new (1, num_cols);
}

/* ============================================== */

#define HDR(NAME)				\
{						\
  BasicCell *hcell;				\
  hcell = xaccMallocTextCell();			\
  reg->header_label_cells[NAME##_CELL] = hcell;	\
}

#define NEW(CN,TYPE)				\
   reg->CN##Cell = xaccMalloc##TYPE##Cell();	\

static void 
xaccInitSplitRegister (SplitRegister *reg,
                       SplitRegisterType type,
                       SplitRegisterStyle style,
                       TableGetEntryHandler entry_handler,
                       VirtCellDataAllocator allocator,
                       VirtCellDataDeallocator deallocator,
                       VirtCellDataCopy copy)
{
  Table * table;
  CellBlock *header;

  reg->table = NULL;
  reg->user_data = NULL;
  reg->destroy = NULL;
  reg->type = type;
  reg->style = style;

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
  HDR (SHRS);
  HDR (SHRBALN);
  HDR (BALN);

  /* --------------------------- */
  /* malloc the workhorse cells */

  NEW (null,    Basic);
  NEW (date,    Date);
  NEW (num,     Num);
  NEW (desc,    QuickFill);
  NEW (recn,    Recn);
  NEW (shrbaln, Price);
  NEW (balance, Price);

  NEW (xfrm,    Combo);
  NEW (mxfrm,   Combo);
  NEW (xto,     Combo);
  NEW (action,  Combo);
  NEW (memo,    QuickFill);
  NEW (credit,  Price);
  NEW (debit,   Price);
  NEW (price,   Price);
  NEW (shares,  Price);

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

  reg->nullCell->input_output = XACC_CELL_ALLOW_NONE;
  xaccSetBasicCellValue (reg->nullCell, "");

  /* The num cell is the transaction number */
  xaccSetBasicCellBlankHelp (&reg->numCell->cell, NUM_CELL_HELP);

  /* the xfer cells */
  xaccSetBasicCellBlankHelp (&reg->mxfrmCell->cell, XFER_CELL_HELP);
  xaccSetBasicCellBlankHelp (&reg->xfrmCell->cell, XFER_CELL_HELP);
  xaccSetBasicCellBlankHelp (&reg->xtoCell->cell, XFER_TO_CELL_HELP);

  xaccComboCellSetIgnoreString (reg->mxfrmCell, SPLIT_STR);
  xaccComboCellSetIgnoreString (reg->xtoCell, SPLIT_STR);

  xaccComboCellSetIgnoreHelp (reg->mxfrmCell, TOOLTIP_MULTI_SPLIT);
  xaccComboCellSetIgnoreHelp (reg->xtoCell, TOOLTIP_MULTI_SPLIT);

  /* the memo cell */
  xaccSetBasicCellBlankHelp (&reg->memoCell->cell, MEMO_CELL_HELP);

  /* the desc cell */
  xaccSetBasicCellBlankHelp (&reg->descCell->cell, DESC_CELL_HELP);

  /* The balance cells are just placeholders */
  reg->balanceCell->cell.input_output = XACC_CELL_ALLOW_NONE;
  reg->shrbalnCell->cell.input_output = XACC_CELL_ALLOW_NONE;

  /* by default, don't blank zeros on the price cells. */
  xaccSetPriceCellBlankZero(reg->priceCell, FALSE);

  /* The reconcile cell should only be entered with the pointer, and
   * only then when the user clicks directly on the cell.  */
  reg->recnCell->cell.input_output |= XACC_CELL_ALLOW_EXACT_ONLY;

  /* Initialize price cells */
  xaccSetPriceCellValue (reg->debitCell, 0.0);
  xaccSetPriceCellValue (reg->creditCell, 0.0);
  xaccSetPriceCellValue (reg->sharesCell, 0.0);

  /* Initialize shares and share balance cells */
  xaccSetPriceCellSharesValue (reg->sharesCell, TRUE);
  xaccSetPriceCellSharesValue (reg->shrbalnCell, TRUE);

  /* The action cell should accept strings not in the list */
  xaccComboCellSetStrict (reg->actionCell, FALSE);
  xaccSetBasicCellBlankHelp (&reg->actionCell->cell, ACTION_CELL_HELP);

  /* number format for share quantities in stock ledgers */
  switch (type)
  {
    case CURRENCY_REGISTER:
      xaccSetPriceCellIsCurrency (reg->priceCell, TRUE);

    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
      xaccSetPriceCellIsCurrency (reg->priceCell, TRUE);

      xaccSetBasicCellBlankHelp (&reg->priceCell->cell, PRICE_CELL_HELP);
      xaccSetBasicCellBlankHelp (&reg->sharesCell->cell, SHARES_CELL_HELP);
      break;
    default:
      break;
  }

  /* add menu items for the action cell */
  configAction (reg);

  reg->cursor_virt_row = 1;
  reg->num_virt_rows = 2;  /* one header, one single_cursor */

  table = gnc_table_new (entry_handler, reg, allocator, deallocator, copy);
  gnc_table_set_size (table, reg->num_virt_rows, 1);

  /* Set up header */
  {
    VirtualCellLocation vcell_loc = { 0, 0 };

    gnc_table_set_cursor (table, header, vcell_loc);
  }

  /* Set up first and only cursor */
  {
    VirtualLocation vloc;

    vloc.vcell_loc.virt_row = reg->cursor_virt_row;
    vloc.vcell_loc.virt_col = 0;
    vloc.phys_row_offset = 0;
    vloc.phys_col_offset = 0;

    gnc_table_set_cursor (table, reg->single_cursor, vloc.vcell_loc);
    gnc_table_move_cursor (table, vloc);
  }

  reg->table = table;

  configTable(reg);
}

/* ============================================== */

void
xaccConfigSplitRegister (SplitRegister *reg,
                         SplitRegisterType newtype,
                         SplitRegisterStyle newstyle)
{
  if (!reg) return;

  reg->type = newtype;
  reg->style = newstyle;

  /* Make sure that any GUI elements associated with this reconfig 
   * are properly initialized. */
  gnc_table_create_cursor (reg->table, reg->single_cursor);
  gnc_table_create_cursor (reg->table, reg->double_cursor);
  gnc_table_create_cursor (reg->table, reg->trans_cursor);
  gnc_table_create_cursor (reg->table, reg->split_cursor);

  configTable(reg);
}

/* ============================================== */

void 
xaccDestroySplitRegister (SplitRegister *reg)
{
  /* give the user a chance to clean up */
  if (reg->destroy)
    (reg->destroy) (reg);

  reg->destroy = NULL;
  reg->user_data = NULL;

  gnc_table_destroy (reg->table);
  reg->table = NULL;

  gnc_cellblock_destroy (reg->header);
  gnc_cellblock_destroy (reg->single_cursor);
  gnc_cellblock_destroy (reg->double_cursor);
  gnc_cellblock_destroy (reg->trans_cursor);
  gnc_cellblock_destroy (reg->split_cursor);

  reg->header = NULL;
  reg->single_cursor = NULL;
  reg->double_cursor = NULL;
  reg->trans_cursor = NULL;
  reg->split_cursor = NULL;

  xaccDestroyDateCell      (reg->dateCell);
  xaccDestroyNumCell       (reg->numCell);
  xaccDestroyQuickFillCell (reg->descCell);
  xaccDestroyRecnCell      (reg->recnCell);
  xaccDestroyPriceCell     (reg->shrbalnCell);
  xaccDestroyPriceCell     (reg->balanceCell);

  xaccDestroyComboCell     (reg->actionCell);
  xaccDestroyComboCell     (reg->xfrmCell);
  xaccDestroyComboCell     (reg->mxfrmCell);
  xaccDestroyComboCell     (reg->xtoCell);
  xaccDestroyQuickFillCell (reg->memoCell);
  xaccDestroyPriceCell     (reg->creditCell);
  xaccDestroyPriceCell     (reg->debitCell);
  xaccDestroyPriceCell     (reg->priceCell);
  xaccDestroyPriceCell     (reg->sharesCell);

  reg->dateCell    = NULL;
  reg->numCell     = NULL;
  reg->descCell    = NULL;
  reg->recnCell    = NULL;
  reg->shrbalnCell = NULL;
  reg->balanceCell = NULL;

  reg->actionCell  = NULL;
  reg->xfrmCell    = NULL;
  reg->mxfrmCell   = NULL;
  reg->xtoCell     = NULL;
  reg->memoCell    = NULL;
  reg->creditCell  = NULL;
  reg->debitCell   = NULL;
  reg->priceCell   = NULL;
  reg->sharesCell  = NULL;

  /* free the memory itself */
  g_free (reg);
}

/* ============================================== */

guint32
xaccSplitRegisterGetChangeFlag (SplitRegister *reg)
{
  guint32 changed = 0;

  /* be careful to use bitwise ands and ors to assemble bit flag */
  changed |= MOD_DATE  & reg->dateCell->cell.changed;
  changed |= MOD_NUM   & reg->numCell->cell.changed;
  changed |= MOD_DESC  & reg->descCell->cell.changed;
  changed |= MOD_RECN  & reg->recnCell->cell.changed;

  changed |= MOD_ACTN  & reg->actionCell->cell.changed;
  changed |= MOD_XFRM  & reg->xfrmCell->cell.changed;
  changed |= MOD_MXFRM & reg->mxfrmCell->cell.changed;
  changed |= MOD_XTO   & reg->xtoCell->cell.changed; 
  changed |= MOD_MEMO  & reg->memoCell->cell.changed;
  changed |= MOD_AMNT  & reg->creditCell->cell.changed;
  changed |= MOD_AMNT  & reg->debitCell->cell.changed;
  changed |= MOD_PRIC  & reg->priceCell->cell.changed;
  changed |= MOD_SHRS  & reg->sharesCell->cell.changed; 

  return changed;
}

/* ============================================== */

void
xaccSplitRegisterClearChangeFlag (SplitRegister *reg)
{
   reg->dateCell->cell.changed = 0;
   reg->numCell->cell.changed = 0;
   reg->descCell->cell.changed = 0;
   reg->recnCell->cell.changed = 0;

   reg->actionCell->cell.changed = 0;
   reg->xfrmCell->cell.changed = 0;
   reg->mxfrmCell->cell.changed = 0;
   reg->xtoCell->cell.changed = 0;
   reg->memoCell->cell.changed = 0;
   reg->creditCell->cell.changed = 0;
   reg->debitCell->cell.changed = 0;
   reg->priceCell->cell.changed = 0;
   reg->sharesCell->cell.changed = 0;
}

/* ============================================== */

static CursorClass
sr_cellblock_cursor_class(SplitRegister *reg, CellBlock *cursor)
{
  if (cursor == NULL)
    return CURSOR_NONE;

  if ((cursor == reg->single_cursor) ||
      (cursor == reg->double_cursor) ||
      (cursor == reg->trans_cursor))
    return CURSOR_TRANS;

  if (cursor == reg->split_cursor)
    return CURSOR_SPLIT;

  return CURSOR_NONE;
}

/* ============================================== */

CursorClass
xaccSplitRegisterGetCurrentCursorClass (SplitRegister *reg)
{
  Table *table;

  if (reg == NULL)
    return CURSOR_NONE;

  table = reg->table;
  if (table == NULL)
    return CURSOR_NONE;

  return sr_cellblock_cursor_class(reg, table->current_cursor);
}

/* ============================================== */

CursorClass
xaccSplitRegisterGetCursorClass (SplitRegister *reg,
                                 VirtualCellLocation vcell_loc)
{
  VirtualCell *vcell;
  Table *table;

  if (reg == NULL)
    return CURSOR_NONE;

  table = reg->table;
  if (table == NULL)
    return CURSOR_NONE;

  vcell = gnc_table_get_virtual_cell (table, vcell_loc);
  if (vcell == NULL)
    return CURSOR_NONE;

  return sr_cellblock_cursor_class(reg, vcell->cellblock);
}

/* ============================================== */

static CellType
sr_cell_type (SplitRegister *reg, void * cell)
{
  if (cell == reg->dateCell)
    return DATE_CELL;

  if (cell == reg->numCell)
    return NUM_CELL;

  if (cell == reg->descCell)
    return DESC_CELL;

  if (cell == reg->recnCell)
    return RECN_CELL;

  if (cell == reg->shrbalnCell)
    return SHRBALN_CELL;

  if (cell == reg->balanceCell)
    return BALN_CELL;

  if (cell == reg->actionCell)
    return ACTN_CELL;

  if (cell == reg->xfrmCell)
    return XFRM_CELL;

  if (cell == reg->mxfrmCell)
    return MXFRM_CELL;

  if (cell == reg->xtoCell)
    return XTO_CELL;

  if (cell == reg->memoCell)
    return MEMO_CELL;

  if (cell == reg->creditCell)
    return CRED_CELL;

  if (cell == reg->debitCell)
    return DEBT_CELL;

  if (cell == reg->priceCell)
    return PRIC_CELL;

  if (cell == reg->sharesCell)
    return SHRS_CELL;

  return NO_CELL;
}

/* ============================================== */

CellType
xaccSplitRegisterGetCurrentCellType (SplitRegister *reg)
{
  Table *table;

  if (reg == NULL)
    return NO_CELL;

  table = reg->table;
  if (table == NULL)
    return NO_CELL;

  return
    xaccSplitRegisterGetCellType(reg, table->current_cursor_loc);
}

/* ============================================== */

static BasicCell *
sr_get_cell (SplitRegister *reg, VirtualLocation virt_loc)
{
  Table *table;
  VirtualCell *vcell;
  CellBlock *cellblock;
  CellBlockCell *cb_cell;

  if (reg == NULL)
    return NULL;

  table = reg->table;
  if (table == NULL)
    return NULL;

  vcell = gnc_table_get_virtual_cell (table, virt_loc.vcell_loc);
  if (vcell == NULL)
    return NULL;

  cellblock = vcell->cellblock;

  cb_cell = gnc_cellblock_get_cell (cellblock,
                                    virt_loc.phys_row_offset,
                                    virt_loc.phys_col_offset);

  if (cb_cell == NULL)
    return NULL;

  return cb_cell->cell;
}

/* ============================================== */

CellType
xaccSplitRegisterGetCellType (SplitRegister *reg, VirtualLocation virt_loc)
{
  BasicCell *cell;

  cell = sr_get_cell (reg, virt_loc);
  if (cell == NULL)
    return NO_CELL;

  return sr_cell_type (reg, cell);
}

/* ============================================== */

gboolean
xaccSplitRegisterGetCellLoc (SplitRegister *reg, CellType cell_type,
                             VirtualCellLocation vcell_loc,
                             VirtualLocation *virt_loc)
{
  Table *table;
  VirtualCell *vcell;
  CellBlock *cellblock;
  int cell_row, cell_col;

  if (reg == NULL)
    return FALSE;

  table = reg->table;
  if (table == NULL)
    return FALSE;

  vcell = gnc_table_get_virtual_cell (table, vcell_loc);
  if (vcell == NULL)
    return FALSE;

  cellblock = vcell->cellblock;

  for (cell_row = 0; cell_row < cellblock->num_rows; cell_row++)
    for (cell_col = 0; cell_col < cellblock->num_cols; cell_col++)
    {
      CellBlockCell *cb_cell;

      cb_cell = gnc_cellblock_get_cell (cellblock, cell_row, cell_col);

      if (sr_cell_type (reg, cb_cell->cell) == cell_type)
      {
        if (virt_loc != NULL)
        {
          virt_loc->vcell_loc = vcell_loc;

          virt_loc->phys_row_offset = cell_row;
          virt_loc->phys_col_offset = cell_col;
        }

        return TRUE;
      }
    }

  return FALSE;
}

/* ============================================== */

gboolean
xaccSplitRegisterGetCurrentCellLoc (SplitRegister *reg, CellType cell_type,
                                    VirtualLocation *virt_loc)
{
  Table *table;

  if (reg == NULL)
    return FALSE;

  table = reg->table;
  if (table == NULL)
    return FALSE;

  return xaccSplitRegisterGetCellLoc (reg, cell_type,
                                      table->current_cursor_loc.vcell_loc,
                                      virt_loc);
}

/* ============================================== */

SplitRegisterBuffer *
xaccMallocSplitRegisterBuffer (void)
{
  SplitRegisterBuffer *srb;

  srb = g_new0(SplitRegisterBuffer, 1);

  return srb;
}

/* ============================================== */

static void
destroyCellBuffer(CellBuffer *cb)
{
  if (cb == NULL)
    return;

  g_free(cb->value);
  cb->value = NULL;
}

void
xaccDestroySplitRegisterBuffer (SplitRegisterBuffer *srb)
{
  if (srb == NULL)
    return;

  destroyCellBuffer(&srb->dateCell);
  destroyCellBuffer(&srb->numCell);
  destroyCellBuffer(&srb->descCell);
  destroyCellBuffer(&srb->recnCell);
  destroyCellBuffer(&srb->shrbalnCell);
  destroyCellBuffer(&srb->balanceCell);
  destroyCellBuffer(&srb->actionCell);
  destroyCellBuffer(&srb->xfrmCell);
  destroyCellBuffer(&srb->mxfrmCell);
  destroyCellBuffer(&srb->xtoCell);
  destroyCellBuffer(&srb->memoCell);
  destroyCellBuffer(&srb->creditCell);
  destroyCellBuffer(&srb->debitCell);
  destroyCellBuffer(&srb->priceCell);
  destroyCellBuffer(&srb->sharesCell);

  g_free(srb);
}

/* ============================================== */

static void
saveCell(BasicCell *bcell, CellBuffer *cb)
{
  if ((bcell == NULL) || (cb == NULL))
    return;

  g_free(cb->value);
  cb->value = g_strdup(bcell->value);

  cb->changed = bcell->changed;
}

void
xaccSplitRegisterSaveCursor(SplitRegister *sr, SplitRegisterBuffer *srb)
{
  if ((sr == NULL) || (srb == NULL))
    return;

  saveCell(&sr->dateCell->cell, &srb->dateCell);
  saveCell(&sr->numCell->cell, &srb->numCell);
  saveCell(&sr->descCell->cell, &srb->descCell);
  saveCell(&sr->recnCell->cell, &srb->recnCell);
  saveCell(&sr->shrbalnCell->cell, &srb->shrbalnCell);
  saveCell(&sr->balanceCell->cell, &srb->balanceCell);
  saveCell(&sr->actionCell->cell, &srb->actionCell);
  saveCell(&sr->xfrmCell->cell, &srb->xfrmCell);
  saveCell(&sr->mxfrmCell->cell, &srb->mxfrmCell);
  saveCell(&sr->xtoCell->cell, &srb->xtoCell);
  saveCell(&sr->memoCell->cell, &srb->memoCell);
  saveCell(&sr->creditCell->cell, &srb->creditCell);
  saveCell(&sr->debitCell->cell, &srb->debitCell);
  saveCell(&sr->priceCell->cell, &srb->priceCell);
  saveCell(&sr->sharesCell->cell, &srb->sharesCell);
}

/* ============================================== */

static void
restoreCellChanged(BasicCell *bcell, CellBuffer *cb)
{
  if ((bcell == NULL) || (cb == NULL))
    return;

  if (cb->changed)
  {
    xaccSetBasicCellValue(bcell, cb->value);
    bcell->changed = cb->changed;
  }
}

void
xaccSplitRegisterRestoreCursorChanged(SplitRegister *sr,
                                      SplitRegisterBuffer *srb)
{
  if ((sr == NULL) || (srb == NULL))
    return;

  restoreCellChanged(&sr->dateCell->cell, &srb->dateCell);
  restoreCellChanged(&sr->numCell->cell, &srb->numCell);
  restoreCellChanged(&sr->descCell->cell, &srb->descCell);
  restoreCellChanged(&sr->recnCell->cell, &srb->recnCell);
  restoreCellChanged(&sr->shrbalnCell->cell, &srb->shrbalnCell);
  restoreCellChanged(&sr->balanceCell->cell, &srb->balanceCell);
  restoreCellChanged(&sr->actionCell->cell, &srb->actionCell);
  restoreCellChanged(&sr->xfrmCell->cell, &srb->xfrmCell);
  restoreCellChanged(&sr->mxfrmCell->cell, &srb->mxfrmCell);
  restoreCellChanged(&sr->xtoCell->cell, &srb->xtoCell);
  restoreCellChanged(&sr->memoCell->cell, &srb->memoCell);
  restoreCellChanged(&sr->creditCell->cell, &srb->creditCell);
  restoreCellChanged(&sr->debitCell->cell, &srb->debitCell);
  restoreCellChanged(&sr->priceCell->cell, &srb->priceCell);
  restoreCellChanged(&sr->sharesCell->cell, &srb->sharesCell);
}

/* keep in sync with CellType enum */
static const char *cell_names[] =
{
  "date",
  "num",
  "description",
  "reconcile",
  "share-balance",
  "balance",
  "action",
  "account",
  "split-account",
  "memo",
  "credit",
  "debit",
  "price",
  "shares",
  "transfer"
};

const char *
xaccSplitRegisterGetCellTypeName (CellType type)
{
  if (type < 0)
    return NULL;
  if (type >= CELL_TYPE_COUNT)
    return NULL;

  return cell_names[type];
}

CellType
xaccSplitRegisterGetCellTypeFromName (const char *name)
{
  CellType type;

  if (name == NULL)
    return NO_CELL;

  for (type = 0; type < CELL_TYPE_COUNT; type++)
    if (safe_strcmp (name, cell_names[type]) == 0)
      return type;

  return NO_CELL;
}

/* ============ END OF FILE ===================== */
