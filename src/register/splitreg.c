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
#include "splitreg.h"
#include "table-allgui.h"

/* FIXME: these shouldn't be here */
#include "combocell.h"
#include "pricecell.h"
#include "recncell.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

typedef struct cell_node
{
  CellType cell_type;
  BasicCell *cell;
} CellNode;

typedef struct _CellBuffer CellBuffer;
struct _CellBuffer
{
  CellType cell_type;
  char * value;
  guint32 changed;
  guint32 conditionally_changed;
};

struct _RegisterBuffer
{
  GList *buffers;
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
  { N_("sample:999,999.000"), 7},                    /* tbalance cell */
  { N_("sample:Notes field sample text string"), 7}, /* notes cell */
  { N_("sample:(x + 0.33 * y + (x+y) )"), 7 },       /* formula credit cell */
  { N_("sample:(x + 0.33 * y + (x+y) )"), 7 },       /* formula debit cell */
};

static CellAlignment cell_alignments[] =
{
  CELL_ALIGN_RIGHT,  /* date cell */
  CELL_ALIGN_LEFT,   /* num cell */
  CELL_ALIGN_LEFT,   /* desc cell */
  CELL_ALIGN_CENTER, /* recn cell */
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
  CELL_ALIGN_RIGHT,  /* tbalance cell */
  CELL_ALIGN_LEFT,   /* notes cell */
  CELL_ALIGN_LEFT,   /* formula credit cell */
  CELL_ALIGN_LEFT,   /* formula debit cell */
};


static void
xaccInitSplitRegister (SplitRegister *reg,
                       SplitRegisterType type,
                       SplitRegisterStyle style,
                       gboolean use_double_line,
                       TableModel *model,
                       gboolean templateMode);


static void
gnc_register_add_cell (SplitRegister *sr,
                       CellType cell_type,
                       const char *cell_type_name)
{
  BasicCell *cell;
  CellNode *node;

  g_return_if_fail (sr != NULL);
  g_return_if_fail (cell_type_name != NULL);

  cell = gnc_register_make_cell (cell_type_name);

  node = g_new0 (CellNode, 1);

  node->cell_type = cell_type;
  node->cell = cell;

  sr->cells = g_list_prepend (sr->cells, node);
}

BasicCell *
gnc_register_get_cell (SplitRegister *sr, CellType cell_type)
{
  GList *node;

  g_return_val_if_fail (sr != NULL, NULL);

  for (node = sr->cells; node; node = node->next)
  {
    CellNode *cn = node->data;

    if (cn->cell_type == cell_type)
      return cn->cell;
  }

  return NULL;
}

const char *
gnc_register_get_cell_value (SplitRegister *sr, CellType cell_type)
{
  BasicCell *cell;

  cell = gnc_register_get_cell (sr, cell_type);
  if (!cell) return NULL;

  return gnc_basic_cell_get_value (cell);
}

gboolean
gnc_register_get_cursor_changed (SplitRegister *sr,
                                 gboolean include_conditional)
{
  GList *node;

  if (!sr) return FALSE;

  for (node = sr->cells; node; node = node->next)
  {
    CellNode *cn = node->data;

    if (gnc_basic_cell_get_changed (cn->cell))
      return TRUE;

    if (include_conditional &&
        gnc_basic_cell_get_conditionally_changed (cn->cell))
      return TRUE;
  }

  return FALSE;
}

gboolean
gnc_register_get_cell_changed (SplitRegister *sr,
                               CellType cell_type,
                               gboolean include_conditional)
{
  BasicCell *cell;

  if (!sr) return FALSE;

  cell = gnc_register_get_cell (sr, cell_type);
  if (!cell) return FALSE;

  if (!include_conditional)
    return gnc_basic_cell_get_changed (cell);
  else
    return (gnc_basic_cell_get_changed (cell) ||
            gnc_basic_cell_get_conditionally_changed (cell));
}

void
gnc_register_clear_changes (SplitRegister *sr)
{
  GList *node;

  if (!sr) return;

  for (node = sr->cells; node; node = node->next)
  {
    CellNode *cn = node->data;

    gnc_basic_cell_set_changed (cn->cell, FALSE);
    gnc_basic_cell_set_conditionally_changed (cn->cell, FALSE);
  }
}

/* ============================================== */
/* configAction strings into the action cell */
/* hack alert -- this stuff really, really should be in a config file ... */

static void
configAction (SplitRegister *reg)
{
  ComboCell *cell;

  cell = (ComboCell *) gnc_register_get_cell (reg, ACTN_CELL);

  /* setup strings in the action pull-down */
  switch (reg->type)
  {
    case BANK_REGISTER:
      /* broken ! FIXME bg */
    case SEARCH_LEDGER:  
      xaccAddComboCellMenuItem (cell, _("Deposit"));
      xaccAddComboCellMenuItem (cell, _("Withdraw"));
      xaccAddComboCellMenuItem (cell, _("Check"));
      xaccAddComboCellMenuItem (cell, _("Int"));
      xaccAddComboCellMenuItem (cell, _("ATM"));
      xaccAddComboCellMenuItem (cell, _("Teller"));
      /* Action: Point Of Sale */
      xaccAddComboCellMenuItem (cell, _("POS"));
      xaccAddComboCellMenuItem (cell, _("Phone"));
      xaccAddComboCellMenuItem (cell, _("Online"));
      /* Action: Automatic Deposit ?!? */
      xaccAddComboCellMenuItem (cell, _("AutoDep"));
      xaccAddComboCellMenuItem (cell, _("Wire"));
      xaccAddComboCellMenuItem (cell, _("Credit"));
      xaccAddComboCellMenuItem (cell, _("Direct Debit"));
      xaccAddComboCellMenuItem (cell, _("Transfer"));
      break;
    case CASH_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      break;
    case ASSET_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Fee"));
      break;
    case CREDIT_REGISTER:
      xaccAddComboCellMenuItem (cell, _("ATM"));
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Credit"));
      xaccAddComboCellMenuItem (cell, _("Fee"));
      xaccAddComboCellMenuItem (cell, _("Int"));
      xaccAddComboCellMenuItem (cell, _("Online"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      break;
    case LIABILITY_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Loan"));
      xaccAddComboCellMenuItem (cell, _("Int"));
      xaccAddComboCellMenuItem (cell, _("Payment"));
      break;
    case INCOME_LEDGER:
    case INCOME_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Int"));
      xaccAddComboCellMenuItem (cell, _("Payment"));
      xaccAddComboCellMenuItem (cell, _("Rebate"));
      break;
    case EXPENSE_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      break;
    case GENERAL_LEDGER:
    case EQUITY_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Equity"));
      break;
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
    case CURRENCY_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Price"));
      xaccAddComboCellMenuItem (cell, _("Fee"));
      /* Action: Dividend */
      xaccAddComboCellMenuItem (cell, _("Div")); 
      xaccAddComboCellMenuItem (cell, _("Int"));
      /* Action: Long Term Capital Gains */
      xaccAddComboCellMenuItem (cell, _("LTCG"));
      /* Action: Short Term Capital Gains */
      xaccAddComboCellMenuItem (cell, _("STCG"));
      xaccAddComboCellMenuItem (cell, _("Income"));
      /* Action: Distribution */
      xaccAddComboCellMenuItem (cell, _("Dist")); 
      xaccAddComboCellMenuItem (cell, _("Split"));
      break;

    default:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      break;
  }
}

/* ============================================== */

static void
set_cell (SplitRegister *reg, CellBlock *cursor,
          CellType cell_type, short row, short col)
{
  CellBlockCell *cb_cell;
  sample_string *ss;

  ss = &cell_sample_strings[cell_type];

  cursor->start_col = MIN (cursor->start_col, col);
  cursor->stop_col  = MAX (cursor->stop_col,  col);

  reg->cursor_header->start_col = MIN (reg->cursor_header->start_col, col);
  reg->cursor_header->stop_col  = MAX (reg->cursor_header->stop_col,  col);

  cb_cell = gnc_cellblock_get_cell (cursor, row, col);

  cb_cell->cell = gnc_register_get_cell (reg, cell_type);
  cb_cell->cell_type = cell_type;
  cb_cell->sample_text = g_strdup (_(ss->string) + ss->offset);
  cb_cell->alignment = cell_alignments[cell_type];
  cb_cell->expandable = cell_type == DESC_CELL;
  cb_cell->span = cell_type == MEMO_CELL || cell_type == NOTES_CELL;

  cb_cell = gnc_cellblock_get_cell (reg->cursor_header, row, col);

  if (cb_cell && (cursor == reg->cursor_ledger_single))
  {
    cb_cell->cell = gnc_register_get_cell (reg, cell_type);
    cb_cell->cell_type = cell_type;
    cb_cell->sample_text = g_strdup (_(ss->string) + ss->offset);
    cb_cell->alignment = cell_alignments[cell_type];
    cb_cell->expandable = cell_type == DESC_CELL;
    cb_cell->span = cell_type == MEMO_CELL || cell_type == NOTES_CELL;
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
        if ( reg->template ) {
                set_cell( reg, curs, FDEBT_CELL, 0, 5);
                set_cell( reg, curs, FCRED_CELL, 0, 6);
        } else {
                set_cell (reg, curs, DEBT_CELL,  0, 5);
                set_cell (reg, curs, CRED_CELL,  0, 6);
        }
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
        if ( reg->template ) {
                set_cell( reg, curs, FDEBT_CELL, 0, 5);
                set_cell( reg, curs, FCRED_CELL, 0, 6);
        } else {
                set_cell (reg, curs, DEBT_CELL, 0, 5);
                set_cell (reg, curs, CRED_CELL, 0, 6);
        }

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
        if ( reg->template ) {
                set_cell (reg, curs, FDEBT_CELL,  0, 5);
                set_cell (reg, curs, FCRED_CELL,  0, 6);
        } else {
                set_cell (reg, curs, DEBT_CELL,  0, 5);
                set_cell (reg, curs, CRED_CELL,  0, 6);
        }

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
        if ( reg->template ) {
                set_cell (reg, curs, FDEBT_CELL,  0, 5);
                set_cell (reg, curs, FCRED_CELL,  0, 6);
        } else {
                set_cell (reg, curs, DEBT_CELL,  0, 5);
                set_cell (reg, curs, CRED_CELL,  0, 6);
        }

        break;
      }

      /* --------------------------------------------------------- */
    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      {
        curs = reg->cursor_ledger_single;
        set_cell (reg, curs, DATE_CELL,  0,  0);
        set_cell (reg, curs, NUM_CELL,   0,  1);
        set_cell (reg, curs, DESC_CELL,  0,  2);
        set_cell (reg, curs, MXFRM_CELL, 0,  3);
        set_cell (reg, curs, RECN_CELL,  0,  4);
        set_cell (reg, curs, SHRS_CELL,  0,  5);
        set_cell (reg, curs, PRIC_CELL,  0,  6);
        set_cell (reg, curs, DEBT_CELL,  0,  7);
        set_cell (reg, curs, CRED_CELL,  0,  8);
        set_cell (reg, curs, BALN_CELL,  0,  9);

        curs = reg->cursor_ledger_double;
        copy_cursor_row (reg, curs, reg->cursor_ledger_single, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = reg->cursor_journal_single;
        set_cell (reg, curs, DATE_CELL,  0,  0);
        set_cell (reg, curs, NUM_CELL,   0,  1);
        set_cell (reg, curs, DESC_CELL,  0,  2);
        set_cell (reg, curs, TSHRS_CELL, 0,  5);
        set_cell (reg, curs, TDEBT_CELL, 0,  7);
        set_cell (reg, curs, TCRED_CELL, 0,  8);
        set_cell (reg, curs, TBALN_CELL, 0,  9);

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
gnc_register_new (SplitRegisterType type,
                  SplitRegisterStyle style,
                  gboolean use_double_line,
                  TableModel *model,
                  gboolean templateMode)
{
  SplitRegister * reg;

  g_return_val_if_fail (model != NULL, NULL);

  reg = g_new0 (SplitRegister, 1);

  model->handler_user_data = reg;

  if (type >= NUM_SINGLE_REGISTER_TYPES)
    style = REG_STYLE_JOURNAL;

  xaccInitSplitRegister (reg,
                         type,
                         style,
                         use_double_line,
                         model,
                         templateMode);

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

static void 
xaccInitSplitRegister (SplitRegister *reg,
                       SplitRegisterType type,
                       SplitRegisterStyle style,
                       gboolean use_double_line,
                       TableModel *model,
                       gboolean templateMode)
{
  Table * table;

  reg->table = NULL;
  reg->user_data = NULL;
  reg->destroy = NULL;

  reg->type = type;
  reg->style = style;
  reg->use_double_line = use_double_line;
  reg->template = templateMode;

  /* --------------------------- */
  /* define the number of columns in the display, malloc the cursors */
  mallocCursors (reg);

  /* --------------------------- */
  /* malloc the workhorse cells */

  reg->nullCell = gnc_register_make_cell (BASIC_CELL_TYPE_NAME);

  gnc_register_add_cell (reg, DATE_CELL,  DATE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, NUM_CELL,   NUM_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, DESC_CELL,  QUICKFILL_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, RECN_CELL,  RECN_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, BALN_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, XFRM_CELL,  COMBO_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, ACTN_CELL,  COMBO_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, MEMO_CELL,  QUICKFILL_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, CRED_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, DEBT_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, PRIC_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, SHRS_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, MXFRM_CELL, COMBO_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, TCRED_CELL, PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, TDEBT_CELL, PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, TSHRS_CELL, PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, TBALN_CELL, PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, NOTES_CELL, QUICKFILL_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, FCRED_CELL, QUICKFILL_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, FDEBT_CELL, QUICKFILL_CELL_TYPE_NAME);

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
  xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, NUM_CELL),
                             _("Enter the transaction number, such as the "
                               "check number"));

  /* the xfer cells */
  {
    const char *help = _("Enter the account to transfer from, or choose "
                         "one from the list");
    xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, MXFRM_CELL), help);
    xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, XFRM_CELL), help);
  }

  {
    const char *help = _("This transaction has multiple splits; "
                         "press the Split button to see them all");

    xaccComboCellAddIgnoreString ((ComboCell *)
                                  gnc_register_get_cell (reg, MXFRM_CELL),
                                  _("-- Split Transaction --"), help);
  }

  {
    const char *help = _("This transaction is a stock split; "
                         "press the Split button to see details");

    xaccComboCellAddIgnoreString ((ComboCell *)
                                  gnc_register_get_cell (reg, MXFRM_CELL),
                                  _("-- Stock Split --"), help);
  }

  /* the action cell */
  xaccComboCellSetAutoSize ((ComboCell *)
                            gnc_register_get_cell (reg, ACTN_CELL), TRUE);

  /* the memo cell */
  xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, MEMO_CELL),
                             _("Enter a description of the split"));

  /* the desc cell */
  xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, DESC_CELL),
                             _("Enter a description of the transaction"));

  /* the notes cell */
  xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, NOTES_CELL),
                             _("Enter notes for the transaction"));
  /* the formula cell */
  xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, FCRED_CELL),
                             _("Enter credit formula for real transaction"));
  xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, FDEBT_CELL),
                             _("Enter debit formula for real transaction"));

  /* Use 6 decimal places for prices */
  xaccSetPriceCellFraction ((PriceCell *)
                            gnc_register_get_cell (reg, PRIC_CELL), 1000000);

  /* Initialize shares and share balance cells */
  xaccSetPriceCellPrintInfo
    ((PriceCell *) gnc_register_get_cell (reg, SHRS_CELL),
     gnc_default_share_print_info ());
  xaccSetPriceCellPrintInfo
    ((PriceCell *) gnc_register_get_cell (reg, TSHRS_CELL),
     gnc_default_share_print_info ());

  /* The action cell should accept strings not in the list */
  xaccComboCellSetStrict ((ComboCell *)
                          gnc_register_get_cell (reg, ACTN_CELL), FALSE);
  xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, ACTN_CELL),
                             _("Enter the type of transaction, or choose "
                               "one from the list"));

  /* number format for share quantities in stock ledgers */
  switch (type)
  {
    case CURRENCY_REGISTER:
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
      xaccSetPriceCellPrintInfo ((PriceCell *)
                                 gnc_register_get_cell (reg, PRIC_CELL),
                                 gnc_default_price_print_info ());

      xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, PRIC_CELL),
                                 _("Enter the share price"));
      xaccSetBasicCellBlankHelp (gnc_register_get_cell (reg, SHRS_CELL),
                                 _("Enter the number of shares bought or "
                                   "sold"));
      break;
    default:
      break;
  }

  /* add menu items for the action cell */
  configAction (reg);

  table = gnc_table_new (model);

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
  GList *node;

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

  gnc_basic_cell_destroy (reg->nullCell);
  reg->nullCell = NULL;

  for (node = reg->cells; node; node = node->next)
  {
    CellNode *cn = node->data;

    gnc_basic_cell_destroy (cn->cell);
    g_free (cn);
  }

  g_list_free (reg->cells);
  reg->cells = NULL;

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
  GList *node;

  if (reg == NULL)
    return NO_CELL;

  for (node = reg->cells; node; node = node->next)
  {
    CellNode *cn = node->data;

    if (cell == cn->cell)
      return cn->cell_type;
  }

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

RegisterBuffer *
gnc_register_buffer_new (void)
{
  RegisterBuffer *rb;

  rb = g_new0 (RegisterBuffer, 1);

  return rb;
}

/* ============================================== */

static void
destroy_cell_buffer (CellBuffer *cb)
{
  if (cb == NULL)
    return;

  g_free (cb->value);
  cb->value = NULL;

  g_free (cb);
}

static void
gnc_register_buffer_clear (RegisterBuffer *rb)
{
  GList *node;

  if (!rb) return;

  for (node = rb->buffers; node; node = node->next)
  {
    CellBuffer *cb = node->data;

    destroy_cell_buffer (cb);
  }

  g_list_free (rb->buffers);
  rb->buffers = NULL;
}

void
gnc_register_buffer_destroy (RegisterBuffer *rb)
{
  if (!rb) return;

  gnc_register_buffer_clear (rb);

  g_free (rb);
}

/* ============================================== */

static CellBuffer *
save_cell (BasicCell *bcell)
{
  CellBuffer *cb;

  if (!bcell)
    return NULL;

  cb = g_new0 (CellBuffer, 1);

  cb->value = g_strdup (bcell->value);
  cb->changed = bcell->changed;
  cb->conditionally_changed = bcell->conditionally_changed;

  return cb;
}

void
gnc_register_save_cursor (SplitRegister *sr, RegisterBuffer *rb)
{
  GList *node;

  if ((sr == NULL) || (rb == NULL))
    return;

  gnc_register_buffer_clear (rb);

  for (node = sr->cells; node; node = node->next)
  {
    CellNode *cn = node->data;
    CellBuffer *cb;

    if (!gnc_basic_cell_get_changed (cn->cell) &&
        !gnc_basic_cell_get_conditionally_changed (cn->cell))
      continue;

    cb = save_cell (cn->cell);
    cb->cell_type = cn->cell_type;

    rb->buffers = g_list_prepend (rb->buffers, cb);
  }
}

/* ============================================== */

static void
restore_cell (BasicCell *bcell, CellBuffer *cb, CellBlock *cursor)
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
gnc_register_restore_cursor (SplitRegister *sr, RegisterBuffer *rb)
{
  CellBlock *cursor;
  GList *node;

  if ((sr == NULL) || (sr->table == NULL) || (rb == NULL))
    return;

  cursor = sr->table->current_cursor;
  if (cursor == NULL)
    return;

  for (node = rb->buffers; node; node = node->next)
  {
    CellBuffer *cb = node->data;
    BasicCell *cell;

    cell = gnc_register_get_cell (sr, cb->cell_type);

    restore_cell (cell, cb, cursor);
  }
}

/* keep in sync with CellType enum */
static const char *cell_names[] =
{
  "date",
  "num",
  "description",
  "reconcile",
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
  "trans-shares",
  "trans-balance",
  "notes",
  "credit formula",
  "debit formula",
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
