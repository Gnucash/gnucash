/*
 * gncEntryLedgerLayout.c -- Layout for GncEntry ledger
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#define _GNU_SOURCE

#include "config.h"

#include <glib.h>

#include "messages.h"

#include "gncEntryLedgerP.h"
#include "gncEntryLedgerLayout.h"

static void
gnc_register_add_cell (TableLayout *layout,
                       const char *cell_name,
                       const char *cell_type_name,
                       const char *sample_text,
                       CellAlignment alignment,
                       gboolean expandable,
                       gboolean span)
{
  BasicCell *cell;

  g_return_if_fail (layout != NULL);
  g_return_if_fail (cell_type_name != NULL);

  cell = gnc_register_make_cell (cell_type_name);

  gnc_basic_cell_set_name (cell, cell_name);
  gnc_basic_cell_set_sample_text (cell, sample_text);
  gnc_basic_cell_set_alignment (cell, alignment);
  gnc_basic_cell_set_expandable (cell, expandable);
  gnc_basic_cell_set_span (cell, span);

  gnc_table_layout_add_cell (layout, cell);
}

static void gnc_entry_ledger_layout_add_cells (GncEntryLedger *ledger,
					       TableLayout *layout)
{
  struct cell_list {
    const char *cell_name;
    const char *cell_type_name;
    const char *sample_text;
    CellAlignment alignment;
    gboolean expandable;
    gboolean span;
  } cells[] = {
    { ENTRY_DATE_CELL, DATE_CELL_TYPE_NAME, N_("sample:12/12/2000")+7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_DESC_CELL, QUICKFILL_CELL_TYPE_NAME,
      N_("sample:Description of an Entry")+7, CELL_ALIGN_LEFT, TRUE, FALSE },
    { ENTRY_ACTN_CELL, COMBO_CELL_TYPE_NAME,
      N_("sample:Action")+7, CELL_ALIGN_RIGHT,
      FALSE, FALSE },
    { ENTRY_QTY_CELL, PRICE_CELL_TYPE_NAME, N_("sample:9,999.00") + 7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_PRIC_CELL, PRICE_CELL_TYPE_NAME, N_("sample:999,999.00") + 7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_TAX_CELL, PRICE_CELL_TYPE_NAME, N_("sample:9,999.00") + 7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_DISC_CELL, PRICE_CELL_TYPE_NAME, N_("sample:9,999.00") + 7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_ACCT_CELL, COMBO_CELL_TYPE_NAME, N_("sample:Xfer:Account")+7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_TAXACC_CELL, COMBO_CELL_TYPE_NAME, N_("sample:Tax:Account")+7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_TAXTYPE_CELL, RECN_CELL_TYPE_NAME, N_("sample:TT")+7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_DISTYPE_CELL, RECN_CELL_TYPE_NAME, N_("sample(DT):+%")+11,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_INV_CELL, RECN_CELL_TYPE_NAME, N_("sample:X")+7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_VALUE_CELL, PRICE_CELL_TYPE_NAME, N_("sample:999,999.00")+7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
    { ENTRY_TAXVAL_CELL, PRICE_CELL_TYPE_NAME, N_("sample:999.00")+7,
      CELL_ALIGN_RIGHT, FALSE, FALSE },
  };
  int i;

  for (i = 0; i < (sizeof(cells)/sizeof(*cells)); i++)
    gnc_register_add_cell (layout, cells[i].cell_name, cells[i].cell_type_name,
			   cells[i].sample_text, cells[i].alignment,
			   cells[i].expandable, cells[i].span);
}

static void gnc_entry_ledger_layout_add_cursors (GncEntryLedger *ledger,
						 TableLayout *layout)
{
  CellBlock *cursor;
  int num_cols;

  switch (ledger->type) {
  case GNCENTRY_ORDER_ENTRY:
  case GNCENTRY_ORDER_VIEWER:
  case GNCENTRY_INVOICE_ENTRY:
  case GNCENTRY_INVOICE_VIEWER:
    num_cols = 14;
    break;
  default:
    g_assert (FALSE);
    return;    
  }

  cursor = gnc_cellblock_new (1, num_cols, CURSOR_HEADER);
  gnc_table_layout_add_cursor (layout, cursor);

  cursor = gnc_cellblock_new (1, num_cols, "cursor");
  gnc_table_layout_add_cursor (layout, cursor);
  gnc_table_layout_set_primary_cursor (layout, cursor);
}

static void gnc_entry_ledger_set_cells (GncEntryLedger *ledger,
					TableLayout *layout)
{
  CellBlock *curs;
  int x = 0;

  switch (ledger->type) {
  case GNCENTRY_ORDER_ENTRY:
  case GNCENTRY_ORDER_VIEWER:
  case GNCENTRY_INVOICE_ENTRY:
  case GNCENTRY_INVOICE_VIEWER:

    curs = gnc_table_layout_get_cursor (layout, "cursor");
    gnc_table_layout_set_cell (layout, curs, ENTRY_INV_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_DATE_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_DESC_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_ACTN_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_ACCT_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_QTY_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_PRIC_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_DISTYPE_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_DISC_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_TAXTYPE_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_TAX_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_TAXACC_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_VALUE_CELL, 0, x++);
    gnc_table_layout_set_cell (layout, curs, ENTRY_TAXVAL_CELL, 0, x++);

    break;

  default:
    g_assert (FALSE);
    return;
  }
}

TableLayout * gnc_entry_ledger_layout_new (GncEntryLedger *ledger)
{
  TableLayout *layout;

  layout = gnc_table_layout_new ();
  gnc_entry_ledger_layout_add_cells (ledger, layout);
  gnc_entry_ledger_layout_add_cursors (ledger, layout);
  gnc_entry_ledger_set_cells (ledger, layout);

  return layout;
}


