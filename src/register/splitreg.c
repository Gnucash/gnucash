/********************************************************************\
 * splitreg.c -- general ledger object build on top of table object *
 *                                                                  *
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

#include <glib.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>

#include "gnc-engine-util.h"
#include "messages.h"
#include "recncell.h"
#include "splitreg.h"
#include "table-allgui.h"
#include "textcell.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

typedef struct _CellBuffer CellBuffer;
struct _CellBuffer
{
  char * value;
  guint32 changed;
  guint32 conditionally_changed;
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
  CellBuffer memoCell;
  CellBuffer creditCell;
  CellBuffer debitCell;
  CellBuffer priceCell;
  CellBuffer sharesCell;
  CellBuffer mxfrmCell;
  CellBuffer notesCell;
};

typedef struct
{
  const char *string;
  int offset;
} sample_string;

static sample_string cell_sample_strings[] =
{
  { N_("sample: 12/12/2000"), 7},                    /* date cell */
  { N_("sample:99999"), 7},                          /* num cell */
  { N_("sample:Description of a transaction"), 7},   /* desc cell */
  { N_("Reconciled:R"), 11},                         /* recn cell */
  { N_("sample:999,999.000"), 7},                    /* share balance cell */
  { N_("sample:999,999.000"), 7},                    /* balance cell */
  { N_("Transfer"), 0},                              /* action cell */
  { N_("sample:Expenses:Automobile:Gasoline"), 7},   /* xfrm cell */
  { N_("sample:Memo field sample text string"), 7},  /* memo cell */
  { N_("sample:999,999.000"), 7},                    /* credit cell */
  { N_("sample:999,999.000"), 7},                    /* debit cell */
  { N_("sample:999,999.000"), 7},                    /* price cell */
  { N_("sample:999,999.000"), 7},                    /* shares cell */
  { N_("sample:Expenses:Automobile:Gasoline"), 7},   /* mxfrm cell */
  { N_("sample:999,999.000"), 7},                    /* tcredit cell */
  { N_("sample:999,999.000"), 7},                    /* tdebit cell */
  { N_("sample:999,999.000"), 7},                    /* tshares cell */
  { N_("sample:999,999.000"), 7},                    /* tshrbaln cell */
  { N_("sample:999,999.000"), 7},                    /* tbalance cell */
  { N_("sample:Notes field sample text string"), 7}, /* notes cell */
};

static CellAlignment cell_alignments[] =
{
  CELL_ALIGN_RIGHT,  /* date cell */
  CELL_ALIGN_LEFT,   /* num cell */
  CELL_ALIGN_LEFT,   /* desc cell */
  CELL_ALIGN_CENTER, /* recn cell */
  CELL_ALIGN_RIGHT,  /* share balance cell */
  CELL_ALIGN_RIGHT,  /* balance cell */
  CELL_ALIGN_LEFT,   /* action cell */
  CELL_ALIGN_RIGHT,  /* xfrm cell */
  CELL_ALIGN_LEFT,   /* memo cell */
  CELL_ALIGN_RIGHT,  /* credit cell */
  CELL_ALIGN_RIGHT,  /* debit cell */
  CELL_ALIGN_RIGHT,  /* price cell */
  CELL_ALIGN_RIGHT,  /* shares cell */
  CELL_ALIGN_RIGHT,  /* mxfrm cell */
  CELL_ALIGN_RIGHT,  /* tcredit cell */
  CELL_ALIGN_RIGHT,  /* tdebit cell */
  CELL_ALIGN_RIGHT,  /* tshares cell */
  CELL_ALIGN_RIGHT,  /* tshrbaln cell */
  CELL_ALIGN_RIGHT,  /* tbalance cell */
  CELL_ALIGN_LEFT,   /* notes cell */
};


static void
xaccInitSplitRegister (SplitRegister *reg,
                       SplitRegisterType type,
                       SplitRegisterStyle style,
                       gboolean use_double_line,
                       TableView *view,
                       VirtCellDataAllocator allocator,
                       VirtCellDataDeallocator deallocator,
                       VirtCellDataCopy copy);


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
      xaccAddComboCellMenuItem (reg->actionCell, _("Deposit"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Withdraw"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Check"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Int"));
      xaccAddComboCellMenuItem (reg->actionCell, _("ATM"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Teller"));
      xaccAddComboCellMenuItem (reg->actionCell, _("POS"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Phone"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Online"));
      xaccAddComboCellMenuItem (reg->actionCell, _("AutoDep"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Wire"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Credit"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Direct Debit"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Transfer"));
      break;
    case CASH_REGISTER:
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      break;
    case ASSET_REGISTER:
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Fee"));
      break;
    case CREDIT_REGISTER:
      xaccAddComboCellMenuItem (reg->actionCell, _("ATM"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Credit"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Fee"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Int"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Online"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      break;
    case LIABILITY_REGISTER:
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Loan"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Int"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Payment"));
      break;
    case INCOME_LEDGER:
    case INCOME_REGISTER:
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Int"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Payment"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Rebate"));
      break;
    case EXPENSE_REGISTER:
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      break;
    case GENERAL_LEDGER:
    case EQUITY_REGISTER:
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Equity"));
      break;
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
    case CURRENCY_REGISTER:
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Price"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Fee"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Div")); /* Dividend */
      xaccAddComboCellMenuItem (reg->actionCell, _("Int"));
      /* Long Term Capital Gains */
      xaccAddComboCellMenuItem (reg->actionCell, _("LTCG"));
      /* Short Term Captial Gains */
      xaccAddComboCellMenuItem (reg->actionCell, _("STCG"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Income"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Dist")); /* Distribution */
      xaccAddComboCellMenuItem (reg->actionCell, _("Split"));
      break;
    default:
      xaccAddComboCellMenuItem (reg->actionCell, _("Buy"));
      xaccAddComboCellMenuItem (reg->actionCell, _("Sell"));
      break;
  }
}

/* ============================================== */

static void
set_cell (SplitRegister *reg, CellBlock *cursor,
          CellType cell_type, short row, short col)
{
  CellBlockCell *cb_cell;
  BasicCell *hcell;
  sample_string *ss;

  ss = &cell_sample_strings[cell_type];

  cursor->start_col = MIN (cursor->start_col, col);
  cursor->stop_col  = MAX (cursor->stop_col,  col);

  reg->cursor_header->start_col = MIN (reg->cursor_header->start_col, col);
  reg->cursor_header->stop_col  = MAX (reg->cursor_header->stop_col,  col);

  hcell = reg->header_cells[cell_type];

  cb_cell = gnc_cellblock_get_cell (cursor, row, col);

  cb_cell->cell = reg->cells[cell_type];
  cb_cell->cell_type = cell_type;
  cb_cell->label = g_strdup (hcell->value);
  cb_cell->sample_text = g_strdup (_(ss->string + ss->offset));
  cb_cell->alignment = cell_alignments[cell_type];
  cb_cell->expandable =
    reg->cells[cell_type] == (BasicCell *) reg->descCell;
  cb_cell->span =
    reg->cells[cell_type] == (BasicCell *) reg->memoCell ||
    reg->cells[cell_type] == (BasicCell *) reg->notesCell;

  cb_cell = gnc_cellblock_get_cell (reg->cursor_header, row, col);

  if (cb_cell && (cursor == reg->cursor_ledger_single))
  {
    cb_cell->cell = reg->cells[cell_type];
    cb_cell->cell_type = cell_type;
    cb_cell->label = g_strdup (hcell->value);
    cb_cell->sample_text = g_strdup (_(ss->string + ss->offset));
    cb_cell->alignment = cell_alignments[cell_type];
    cb_cell->expandable =
      reg->cells[cell_type] == (BasicCell *) reg->descCell;
    cb_cell->span =
      reg->cells[cell_type] == (BasicCell *) reg->memoCell ||
      reg->cells[cell_type] == (BasicCell *) reg->notesCell;
  }
}

static void
copy_cursor_row (SplitRegister *reg, CellBlock *to, CellBlock *from, int row)
{
  int col;

  for (col = 0; col < from->num_cols; col++)
  {
    CellBlockCell *cb_cell;

    cb_cell = gnc_cellblock_get_cell (from, row, col);
    if (cb_cell->cell_type < 0)
      continue;

    set_cell (reg, to, cb_cell->cell_type, row, col);
  }
}

/* ============================================== */

static void
configLayout (SplitRegister *reg)
{
  CellBlock *curs;
  int i;

  /* fill things up with null cells */
  for (i = 0; i < reg->cursor_header->num_cols; i++)
  {
    CellBlockCell *cb_cell;

    cb_cell = gnc_cellblock_get_cell (reg->cursor_header, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->cursor_ledger_single, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->cursor_ledger_double, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->cursor_ledger_double, 1, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->cursor_journal_single, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->cursor_journal_double, 0, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->cursor_journal_double, 1, i);
    cb_cell->cell = reg->nullCell;

    cb_cell = gnc_cellblock_get_cell (reg->cursor_split, 0, i);
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
        curs = reg->cursor_ledger_single;
        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, MXFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL,  0, 4);
        set_cell (reg, curs, DEBT_CELL,  0, 5);
        set_cell (reg, curs, CRED_CELL,  0, 6);
        set_cell (reg, curs, BALN_CELL,  0, 7);

        curs = reg->cursor_ledger_double;
        copy_cursor_row (reg, curs, reg->cursor_ledger_single, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_journal_single;
        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, TDEBT_CELL, 0, 5);
        set_cell (reg, curs, TCRED_CELL, 0, 6);
        set_cell (reg, curs, TBALN_CELL, 0, 7);

        curs = reg->cursor_journal_double;
        copy_cursor_row (reg, curs, reg->cursor_journal_single, 0);

        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_split;
        set_cell (reg, curs, ACTN_CELL, 0, 1);
        set_cell (reg, curs, MEMO_CELL, 0, 2);
        set_cell (reg, curs, XFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL, 0, 4);
        set_cell (reg, curs, DEBT_CELL, 0, 5);
        set_cell (reg, curs, CRED_CELL, 0, 6);

        break;
      }

      /* --------------------------------------------------------- */
    case INCOME_LEDGER:
    case GENERAL_LEDGER:
    case SEARCH_LEDGER:
      {
        curs = reg->cursor_ledger_single;
        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, MXFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL,  0, 4);
        set_cell (reg, curs, DEBT_CELL,  0, 5);
        set_cell (reg, curs, CRED_CELL,  0, 6);

        curs = reg->cursor_ledger_double;
        copy_cursor_row (reg, curs, reg->cursor_ledger_single, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_journal_single;
        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, TDEBT_CELL, 0, 5);
        set_cell (reg, curs, TCRED_CELL, 0, 6);

        curs = reg->cursor_journal_double;
        copy_cursor_row (reg, curs, reg->cursor_journal_single, 0);

        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_split;
        set_cell (reg, curs, ACTN_CELL, 0, 1);
        set_cell (reg, curs, MEMO_CELL, 0, 2);
        set_cell (reg, curs, XFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL, 0, 4);
        set_cell (reg, curs, DEBT_CELL, 0, 5);
        set_cell (reg, curs, CRED_CELL, 0, 6);

        break;
      }

      /* --------------------------------------------------------- */
    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      {
        curs = reg->cursor_ledger_single;
        set_cell (reg, curs, DATE_CELL,    0,  0);
        set_cell (reg, curs, NUM_CELL,     0,  1);
        set_cell (reg, curs, DESC_CELL,    0,  2);
        set_cell (reg, curs, MXFRM_CELL,   0,  3);
        set_cell (reg, curs, RECN_CELL,    0,  4);
        set_cell (reg, curs, SHRS_CELL,    0,  5);
        set_cell (reg, curs, PRIC_CELL,    0,  6);
        set_cell (reg, curs, DEBT_CELL,    0,  7);
        set_cell (reg, curs, CRED_CELL,    0,  8);
        set_cell (reg, curs, SHRBALN_CELL, 0,  9);

        curs = reg->cursor_ledger_double;
        copy_cursor_row (reg, curs, reg->cursor_ledger_single, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_journal_single;
        set_cell (reg, curs, DATE_CELL,     0,  0);
        set_cell (reg, curs, NUM_CELL,      0,  1);
        set_cell (reg, curs, DESC_CELL,     0,  2);
        set_cell (reg, curs, TSHRS_CELL,    0,  5);
        set_cell (reg, curs, TDEBT_CELL,    0,  7);
        set_cell (reg, curs, TCRED_CELL,    0,  8);
        set_cell (reg, curs, TSHRBALN_CELL, 0,  9);

        curs = reg->cursor_journal_double;
        copy_cursor_row (reg, curs, reg->cursor_journal_single, 0);

        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_split;
        set_cell (reg, curs, ACTN_CELL, 0, 1);
        set_cell (reg, curs, MEMO_CELL, 0, 2);
        set_cell (reg, curs, XFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL, 0, 4);
        set_cell (reg, curs, SHRS_CELL, 0, 5);
        set_cell (reg, curs, PRIC_CELL, 0, 6);
        set_cell (reg, curs, DEBT_CELL, 0, 7);
        set_cell (reg, curs, CRED_CELL, 0, 8);

        break;
      }

      /* --------------------------------------------------------- */
    case PORTFOLIO_LEDGER:
      {
        curs = reg->cursor_ledger_single;
        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, MXFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL,  0, 4);
        set_cell (reg, curs, SHRS_CELL,  0, 5);
        set_cell (reg, curs, PRIC_CELL,  0, 6);
        set_cell (reg, curs, DEBT_CELL,  0, 7);
        set_cell (reg, curs, CRED_CELL,  0, 8);

        curs = reg->cursor_ledger_double;
        copy_cursor_row (reg, curs, reg->cursor_ledger_single, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_journal_single;
        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, TSHRS_CELL, 0, 5);
        set_cell (reg, curs, TDEBT_CELL, 0, 7);
        set_cell (reg, curs, TCRED_CELL, 0, 8);

        curs = reg->cursor_journal_double;
        copy_cursor_row (reg, curs, reg->cursor_journal_single, 0);

        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_split;
        set_cell (reg, curs, ACTN_CELL, 0, 1);
        set_cell (reg, curs, MEMO_CELL, 0, 2);
        set_cell (reg, curs, XFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL, 0, 4);
        set_cell (reg, curs, SHRS_CELL, 0, 5);
        set_cell (reg, curs, PRIC_CELL, 0, 6);
        set_cell (reg, curs, DEBT_CELL, 0, 7);
        set_cell (reg, curs, CRED_CELL, 0, 8);

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
                         gboolean use_double_line,
                         TableView *view,
                         VirtCellDataAllocator allocator,
                         VirtCellDataDeallocator deallocator,
                         VirtCellDataCopy copy)
{
  SplitRegister * reg;

  g_return_val_if_fail (view != NULL, NULL);

  reg = g_new0 (SplitRegister, 1);

  view->handler_user_data = reg;

  if (type >= NUM_SINGLE_REGISTER_TYPES)
    style = REG_STYLE_JOURNAL;

  xaccInitSplitRegister (reg,
                         type,
                         style,
                         use_double_line,
                         view,
                         allocator,
                         deallocator,
                         copy);

  return reg;
}

/* ============================================== */

static void
mallocCursors (SplitRegister *reg)
{
  int num_cols;

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
      num_cols = 8;
      break;

    case INCOME_LEDGER:
    case GENERAL_LEDGER:
    case SEARCH_LEDGER:
      num_cols = 7;
      break;

    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      num_cols = 11;
      break;

    case PORTFOLIO_LEDGER:
      num_cols = 9;
      break;

    default:
      PERR("Bad register type");
      g_assert (FALSE);
      return;
  }

  reg->cursor_header = gnc_cellblock_new (1, num_cols, CURSOR_TYPE_HEADER);

  /* cursors used in ledger mode */
  reg->cursor_ledger_single =
    gnc_cellblock_new (1, num_cols, CURSOR_TYPE_SINGLE_LEDGER);
  reg->cursor_ledger_double =
    gnc_cellblock_new (2, num_cols, CURSOR_TYPE_DOUBLE_LEDGER);

  /* cursors used for journal mode */
  reg->cursor_journal_single =
    gnc_cellblock_new (1, num_cols, CURSOR_TYPE_SINGLE_JOURNAL);
  reg->cursor_journal_double =
    gnc_cellblock_new (2, num_cols, CURSOR_TYPE_DOUBLE_JOURNAL);

  reg->cursor_split =
    gnc_cellblock_new (1, num_cols, CURSOR_TYPE_SPLIT);
}

/* ============================================== */

#define NEW(NAME, CN, TYPE)			\
   reg->CN##Cell = xaccMalloc##TYPE##Cell();	\
   reg->cells[NAME##_CELL] = (BasicCell *) reg->CN##Cell;

static void 
xaccInitSplitRegister (SplitRegister *reg,
                       SplitRegisterType type,
                       SplitRegisterStyle style,
                       gboolean use_double_line,
                       TableView *view,
                       VirtCellDataAllocator allocator,
                       VirtCellDataDeallocator deallocator,
                       VirtCellDataCopy copy)
{
  Table * table;

  reg->table = NULL;
  reg->user_data = NULL;
  reg->destroy = NULL;

  reg->type = type;
  reg->style = style;
  reg->use_double_line = use_double_line;

  /* --------------------------- */
  /* define the number of columns in the display, malloc the cursors */
  mallocCursors (reg);

  /* --------------------------- */
  /* malloc the header (label) cells */
  {
    int i;

    for (i = 0; i < CELL_TYPE_COUNT; i++)
      reg->header_cells[i] = xaccMallocTextCell ();
  }

  /* --------------------------- */
  /* malloc the workhorse cells */

  reg->nullCell = xaccMallocBasicCell ();

  NEW (DATE,     date,     Date);
  NEW (NUM,      num,      Num);
  NEW (DESC,     desc,     QuickFill);
  NEW (RECN,     recn,     Recn);
  NEW (SHRBALN,  shrbaln,  Price);
  NEW (BALN,     balance,  Price);
  NEW (XFRM,     xfrm,     Combo);
  NEW (ACTN,     action,   Combo);
  NEW (MEMO,     memo,     QuickFill);
  NEW (CRED,     credit,   Price);
  NEW (DEBT,     debit,    Price);
  NEW (PRIC,     price,    Price);
  NEW (SHRS,     shares,   Price);
  NEW (MXFRM,    mxfrm,    Combo);
  NEW (TCRED,    tcredit,  Price);
  NEW (TDEBT,    tdebit,   Price);
  NEW (TSHRS,    tshares,  Price);
  NEW (TSHRBALN, tshrbaln, Price);
  NEW (TBALN,    tbalance, Price);
  NEW (NOTES,    notes,    QuickFill);

  /* --------------------------- */

  /* config the layout of the cells in the cursors */
  configLayout (reg);

  /* The Null Cell is used to make sure that "empty" cells stay empty.
   * This solves the problem of having the table be reformatted, the
   * result of which is that an empty cell has landed on a cell that
   * was previously non-empty.  We want to make sure that we erase
   * those cell contents. The null cells handles this for us. */
  xaccSetBasicCellValue (reg->nullCell, "");

  /* The num cell is the transaction number */
  xaccSetBasicCellBlankHelp (&reg->numCell->cell,
                             _("Enter the transaction number, such as the "
                               "check number"));

  /* the xfer cells */
  {
    const char *help = _("Enter the account to transfer from, or choose "
                         "one from the list");
    xaccSetBasicCellBlankHelp (&reg->mxfrmCell->cell, help);
    xaccSetBasicCellBlankHelp (&reg->xfrmCell->cell, help);
  }

  {
    const char *help = _("This transaction has multiple splits; "
                         "press the Split button to see them all");

    xaccComboCellAddIgnoreString (reg->mxfrmCell,
                                  _("-- Split Transaction --"), help);
  }

  {
    const char *help = _("This transaction is a stock split; "
                         "press the Split button to see details");

    xaccComboCellAddIgnoreString (reg->mxfrmCell,
                                  _("-- Stock Split --"), help);
  }

  /* the memo cell */
  xaccSetBasicCellBlankHelp (&reg->memoCell->cell,
                             _("Enter a description of the split"));

  /* the desc cell */
  xaccSetBasicCellBlankHelp (&reg->descCell->cell,
                             _("Enter a description of the transaction"));

  /* the notes cell */
  xaccSetBasicCellBlankHelp (&reg->notesCell->cell,
                             _("Enter notes for the transaction"));

  /* Use 5 decimal places for prices */
  xaccSetPriceCellFraction (reg->priceCell, 1000000);

  /* Initialize price cells */
  xaccSetPriceCellValue (reg->debitCell, gnc_numeric_zero ());
  xaccSetPriceCellValue (reg->creditCell, gnc_numeric_zero ());
  xaccSetPriceCellValue (reg->sharesCell, gnc_numeric_zero ());

  /* Initialize shares and share balance cells */
  xaccSetPriceCellPrintInfo
    (reg->sharesCell, gnc_default_share_print_info ());
  xaccSetPriceCellPrintInfo
    (reg->tsharesCell, gnc_default_share_print_info ());
  xaccSetPriceCellPrintInfo
    (reg->shrbalnCell, gnc_default_share_print_info ());
  xaccSetPriceCellPrintInfo
    (reg->tshrbalnCell, gnc_default_share_print_info ());

  /* The action cell should accept strings not in the list */
  xaccComboCellSetStrict (reg->actionCell, FALSE);
  xaccSetBasicCellBlankHelp (&reg->actionCell->cell,
                             _("Enter the type of transaction, or choose "
                               "one from the list"));

  /* number format for share quantities in stock ledgers */
  switch (type)
  {
    case CURRENCY_REGISTER:
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
      xaccSetPriceCellPrintInfo (reg->priceCell,
                                 gnc_default_price_print_info ());

      xaccSetBasicCellBlankHelp (&reg->priceCell->cell,
                                 _("Enter the share price"));
      xaccSetBasicCellBlankHelp (&reg->sharesCell->cell,
                                 _("Enter the number of shares bought or "
                                   "sold"));
      break;
    default:
      break;
  }

  /* add menu items for the action cell */
  configAction (reg);

  table = gnc_table_new (view, allocator, deallocator, copy);

  reg->table = table;

  /* Set up header */
  {
    VirtualCellLocation vcell_loc = { 0, 0 };

    gnc_table_set_vcell (table, reg->cursor_header,
                         NULL, TRUE, TRUE, vcell_loc);

    table->num_header_phys_rows = use_double_line ? 2 : 1;
  }

  /* Set up first and only initial row */
  {
    VirtualLocation vloc;

    vloc.vcell_loc.virt_row = 1;
    vloc.vcell_loc.virt_col = 0;
    vloc.phys_row_offset = 0;
    vloc.phys_col_offset = 0;

    gnc_table_set_vcell (table, reg->cursor_ledger_single,
                         NULL, TRUE, TRUE, vloc.vcell_loc);
    gnc_table_move_cursor (table, vloc);
  }
}

/* ============================================== */

void
xaccConfigSplitRegister (SplitRegister *reg,
                         SplitRegisterType newtype,
                         SplitRegisterStyle newstyle,
                         gboolean use_double_line)
{
  if (!reg) return;

  reg->type = newtype;

  if (reg->type >= NUM_SINGLE_REGISTER_TYPES)
    newstyle = REG_STYLE_JOURNAL;

  reg->style = newstyle;
  reg->use_double_line = use_double_line;

  reg->table->num_header_phys_rows = use_double_line ? 2 : 1;

  /* Make sure that any GUI elements associated with this reconfig 
   * are properly initialized. */
  gnc_table_create_cursor (reg->table, reg->cursor_ledger_single);
  gnc_table_create_cursor (reg->table, reg->cursor_ledger_double);
  gnc_table_create_cursor (reg->table, reg->cursor_journal_single);
  gnc_table_create_cursor (reg->table, reg->cursor_journal_double);
  gnc_table_create_cursor (reg->table, reg->cursor_split);
}

/* ============================================== */

void 
xaccDestroySplitRegister (SplitRegister *reg)
{
  int i;

  /* give the user a chance to clean up */
  if (reg->destroy)
    (reg->destroy) (reg);

  reg->destroy = NULL;
  reg->user_data = NULL;

  gnc_table_destroy (reg->table);
  reg->table = NULL;

  gnc_cellblock_destroy (reg->cursor_header);
  gnc_cellblock_destroy (reg->cursor_ledger_single);
  gnc_cellblock_destroy (reg->cursor_ledger_double);
  gnc_cellblock_destroy (reg->cursor_journal_single);
  gnc_cellblock_destroy (reg->cursor_journal_double);
  gnc_cellblock_destroy (reg->cursor_split);

  reg->cursor_header = NULL;
  reg->cursor_ledger_single = NULL;
  reg->cursor_ledger_double = NULL;
  reg->cursor_journal_single = NULL;
  reg->cursor_journal_double = NULL;
  reg->cursor_split = NULL;

  xaccDestroyBasicCell     (reg->nullCell);
  xaccDestroyDateCell      (reg->dateCell);
  xaccDestroyNumCell       (reg->numCell);
  xaccDestroyQuickFillCell (reg->descCell);
  xaccDestroyRecnCell      (reg->recnCell);
  xaccDestroyPriceCell     (reg->shrbalnCell);
  xaccDestroyPriceCell     (reg->balanceCell);
  xaccDestroyComboCell     (reg->actionCell);
  xaccDestroyComboCell     (reg->xfrmCell);
  xaccDestroyQuickFillCell (reg->memoCell);
  xaccDestroyPriceCell     (reg->creditCell);
  xaccDestroyPriceCell     (reg->debitCell);
  xaccDestroyPriceCell     (reg->priceCell);
  xaccDestroyPriceCell     (reg->sharesCell);
  xaccDestroyComboCell     (reg->mxfrmCell);
  xaccDestroyPriceCell     (reg->tcreditCell);
  xaccDestroyPriceCell     (reg->tdebitCell);
  xaccDestroyPriceCell     (reg->tsharesCell);
  xaccDestroyPriceCell     (reg->tshrbalnCell);
  xaccDestroyPriceCell     (reg->tbalanceCell);
  xaccDestroyQuickFillCell (reg->notesCell);

  reg->nullCell     = NULL;
  reg->dateCell     = NULL;
  reg->numCell      = NULL;
  reg->descCell     = NULL;
  reg->recnCell     = NULL;
  reg->shrbalnCell  = NULL;
  reg->balanceCell  = NULL;
  reg->actionCell   = NULL;
  reg->xfrmCell     = NULL;
  reg->memoCell     = NULL;
  reg->creditCell   = NULL;
  reg->debitCell    = NULL;
  reg->priceCell    = NULL;
  reg->sharesCell   = NULL;
  reg->mxfrmCell    = NULL;
  reg->tcreditCell  = NULL;
  reg->tdebitCell   = NULL;
  reg->tsharesCell  = NULL;
  reg->tshrbalnCell = NULL;
  reg->tbalanceCell = NULL;
  reg->notesCell    = NULL;

  for (i = 0; i < CELL_TYPE_COUNT; i++)
  {
    BasicCell *cell;

    cell = reg->header_cells[i];
    if (cell)
      xaccDestroyTextCell (cell);
    reg->header_cells[i] = NULL;
  }

  g_free (reg->debit_str);
  g_free (reg->tdebit_str);
  g_free (reg->credit_str);
  g_free (reg->tcredit_str);

  reg->debit_str = NULL;
  reg->tdebit_str = NULL;
  reg->credit_str = NULL;
  reg->tcredit_str = NULL;

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
  changed |= MOD_MEMO  & reg->memoCell->cell.changed;
  changed |= MOD_AMNT  & reg->creditCell->cell.changed;
  changed |= MOD_AMNT  & reg->debitCell->cell.changed;
  changed |= MOD_PRIC  & reg->priceCell->cell.changed;
  changed |= MOD_SHRS  & reg->sharesCell->cell.changed; 
  changed |= MOD_MXFRM & reg->mxfrmCell->cell.changed;
  changed |= MOD_NOTES & reg->notesCell->cell.changed;

  return changed;
}

guint32
xaccSplitRegisterGetConditionalChangeFlag (SplitRegister *reg)
{
  guint32 changed = 0;

  /* be careful to use bitwise ands and ors to assemble bit flag */
  changed |= MOD_DATE  & reg->dateCell->cell.conditionally_changed;
  changed |= MOD_NUM   & reg->numCell->cell.conditionally_changed;
  changed |= MOD_DESC  & reg->descCell->cell.conditionally_changed;
  changed |= MOD_RECN  & reg->recnCell->cell.conditionally_changed;
  changed |= MOD_ACTN  & reg->actionCell->cell.conditionally_changed;
  changed |= MOD_XFRM  & reg->xfrmCell->cell.conditionally_changed;
  changed |= MOD_MEMO  & reg->memoCell->cell.conditionally_changed;
  changed |= MOD_AMNT  & reg->creditCell->cell.conditionally_changed;
  changed |= MOD_AMNT  & reg->debitCell->cell.conditionally_changed;
  changed |= MOD_PRIC  & reg->priceCell->cell.conditionally_changed;
  changed |= MOD_SHRS  & reg->sharesCell->cell.conditionally_changed; 
  changed |= MOD_MXFRM & reg->mxfrmCell->cell.conditionally_changed;
  changed |= MOD_NOTES & reg->notesCell->cell.conditionally_changed;

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
   reg->memoCell->cell.changed = 0;
   reg->creditCell->cell.changed = 0;
   reg->debitCell->cell.changed = 0;
   reg->priceCell->cell.changed = 0;
   reg->sharesCell->cell.changed = 0;
   reg->mxfrmCell->cell.changed = 0;
   reg->notesCell->cell.changed = 0;
}

/* ============================================== */

static CursorClass
sr_cellblock_cursor_class(SplitRegister *reg, CellBlock *cursor)
{
  if (cursor == NULL)
    return CURSOR_CLASS_NONE;

  if ((cursor == reg->cursor_ledger_single)  ||
      (cursor == reg->cursor_ledger_double)  ||
      (cursor == reg->cursor_journal_single) ||
      (cursor == reg->cursor_journal_double))
    return CURSOR_CLASS_TRANS;

  if (cursor == reg->cursor_split)
    return CURSOR_CLASS_SPLIT;

  return CURSOR_CLASS_NONE;
}

/* ============================================== */

CursorClass
xaccSplitRegisterGetCurrentCursorClass (SplitRegister *reg)
{
  Table *table;

  if (reg == NULL)
    return CURSOR_CLASS_NONE;

  table = reg->table;
  if (table == NULL)
    return CURSOR_CLASS_NONE;

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
    return CURSOR_CLASS_NONE;

  table = reg->table;
  if (table == NULL)
    return CURSOR_CLASS_NONE;

  vcell = gnc_table_get_virtual_cell (table, vcell_loc);
  if (vcell == NULL)
    return CURSOR_CLASS_NONE;

  return sr_cellblock_cursor_class(reg, vcell->cellblock);
}

/* ============================================== */

CursorClass
xaccCursorTypeToClass (CursorType cursor_type)
{
  switch (cursor_type)
  {
    case CURSOR_TYPE_SINGLE_LEDGER:
    case CURSOR_TYPE_DOUBLE_LEDGER:
    case CURSOR_TYPE_SINGLE_JOURNAL:
    case CURSOR_TYPE_DOUBLE_JOURNAL:
      return CURSOR_CLASS_TRANS;

    case CURSOR_TYPE_SPLIT:
      return CURSOR_CLASS_SPLIT;

    default:
      return CURSOR_CLASS_NONE;
  }
}

/* ============================================== */

static CellType
sr_cell_type (SplitRegister *reg, void * cell)
{
  int i;

  if (reg == NULL)
    return NO_CELL;

  for (i = 0; i < CELL_TYPE_COUNT; i++)
    if (cell == reg->cells[i])
      return i;

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

  return xaccSplitRegisterGetCellType(reg, table->current_cursor_loc);
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
  destroyCellBuffer(&srb->memoCell);
  destroyCellBuffer(&srb->creditCell);
  destroyCellBuffer(&srb->debitCell);
  destroyCellBuffer(&srb->priceCell);
  destroyCellBuffer(&srb->sharesCell);
  destroyCellBuffer(&srb->mxfrmCell);
  destroyCellBuffer(&srb->notesCell);

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
  cb->conditionally_changed = bcell->conditionally_changed;
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
  saveCell(&sr->memoCell->cell, &srb->memoCell);
  saveCell(&sr->creditCell->cell, &srb->creditCell);
  saveCell(&sr->debitCell->cell, &srb->debitCell);
  saveCell(&sr->priceCell->cell, &srb->priceCell);
  saveCell(&sr->sharesCell->cell, &srb->sharesCell);
  saveCell(&sr->mxfrmCell->cell, &srb->mxfrmCell);
  saveCell(&sr->notesCell->cell, &srb->notesCell);
}

/* ============================================== */

static void
restoreCellChanged(BasicCell *bcell, CellBuffer *cb, CellBlock *cursor)
{
  int r, c;

  if ((bcell == NULL) || (cb == NULL))
    return;

  if (!cb->changed && !cb->conditionally_changed)
    return;

  /* only restore if it's in the current cursor */
  for (r = 0; r < cursor->num_rows; r++)
    for (c = 0; c < cursor->num_cols; c++)
    {
      CellBlockCell *cb_cell;

      cb_cell = gnc_cellblock_get_cell (cursor, r, c);
      if (cb_cell == NULL)
        continue;

      if (cb_cell->cell == bcell)
      {
        xaccSetBasicCellValue(bcell, cb->value);
        bcell->changed = cb->changed;
        bcell->conditionally_changed = cb->conditionally_changed;
        return;
      }
    }
}

void
xaccSplitRegisterRestoreCursorChanged(SplitRegister *sr,
                                      SplitRegisterBuffer *srb)
{
  CellBlock *cursor;

  if ((sr == NULL) || (sr->table == NULL) || (srb == NULL))
    return;

  cursor = sr->table->current_cursor;
  if (cursor == NULL)
    return;

  restoreCellChanged(&sr->dateCell->cell, &srb->dateCell, cursor);
  restoreCellChanged(&sr->numCell->cell, &srb->numCell, cursor);
  restoreCellChanged(&sr->descCell->cell, &srb->descCell, cursor);
  restoreCellChanged(&sr->recnCell->cell, &srb->recnCell, cursor);
  restoreCellChanged(&sr->shrbalnCell->cell, &srb->shrbalnCell, cursor);
  restoreCellChanged(&sr->balanceCell->cell, &srb->balanceCell, cursor);
  restoreCellChanged(&sr->actionCell->cell, &srb->actionCell, cursor);
  restoreCellChanged(&sr->xfrmCell->cell, &srb->xfrmCell, cursor);
  restoreCellChanged(&sr->memoCell->cell, &srb->memoCell, cursor);
  restoreCellChanged(&sr->creditCell->cell, &srb->creditCell, cursor);
  restoreCellChanged(&sr->debitCell->cell, &srb->debitCell, cursor);
  restoreCellChanged(&sr->priceCell->cell, &srb->priceCell, cursor);
  restoreCellChanged(&sr->sharesCell->cell, &srb->sharesCell, cursor);
  restoreCellChanged(&sr->mxfrmCell->cell, &srb->mxfrmCell, cursor);
  restoreCellChanged(&sr->notesCell->cell, &srb->notesCell, cursor);
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
  "memo",
  "credit",
  "debit",
  "price",
  "shares",
  "transfer",
  "trans-credit",
  "trans-debit",
  "trans-share-balance",
  "trans-balance",
  "notes"
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
