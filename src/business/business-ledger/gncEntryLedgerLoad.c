/*
 * gncEntryLedgerLoad.c -- a Ledger widget for entering GncEntry objects
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#define _GNU_SOURCE

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "gnc-ui-util.h"
#include "recncell.h"
#include "combocell.h"
#include "messages.h"

#include "gncEntry.h"
#include "gncEntryLedger.h"
#include "gncEntryLedgerP.h"


/* XXX: This should go elsewhere */
const char * gnc_entry_ledger_type_string_getter (char flag)
{
  switch (flag) {
  case '0': return _("$");
  case '1': return _("%");
  case '2': return _("+$");
  case '3': return _("+%");
  default:
    return "?";
  };
}

static void load_tax_type_cells (GncEntryLedger *ledger)
{
  RecnCell *cell;

  if (!ledger) return;

  cell = (RecnCell *)
    gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAXTYPE_CELL);

  if (!cell) return;

  gnc_recn_cell_set_valid_flags (cell, "01", '1');
  gnc_recn_cell_set_flag_order (cell, "01");
  gnc_recn_cell_set_string_getter (cell, gnc_entry_ledger_type_string_getter);
}

static void load_discount_type_cells (GncEntryLedger *ledger)
{
  RecnCell *cell;

  if (!ledger) return;

  cell = (RecnCell *)
    gnc_table_layout_get_cell (ledger->table->layout, ENTRY_DISTYPE_CELL);

  if (!cell) return;

  gnc_recn_cell_set_valid_flags (cell, "0123", '1');
  gnc_recn_cell_set_flag_order (cell, "1032");
  gnc_recn_cell_set_string_getter (cell, gnc_entry_ledger_type_string_getter);
}

static void load_inv_type_cells (GncEntryLedger *ledger)
{
  RecnCell *cell;

  if (!ledger) return;

  cell = (RecnCell *)
    gnc_table_layout_get_cell (ledger->table->layout, ENTRY_INV_CELL);

  if (!cell) return;

  gnc_recn_cell_set_valid_flags (cell, " X", ' ');
  gnc_recn_cell_set_flag_order (cell, " X");
}

static void load_xfer_cell (ComboCell * cell, AccountGroup * grp)
{
  GList *list;
  GList *node;

  if (!grp) return;

  /* Build the xfer menu out of account names. */

  list = xaccGroupGetSubAccounts (grp);

  for (node = list; node; node = node->next) {
    Account *account = node->data;
    char *name;

    name = xaccAccountGetFullName (account, gnc_get_account_separator ());
    if (name != NULL) {
      gnc_combo_cell_add_menu_item (cell, name);
      g_free(name);
    }
  }

  g_list_free (list);
}

static void load_xfer_type_cells (GncEntryLedger *ledger)
{
  AccountGroup *group;
  ComboCell *cell;

  group = gnc_book_get_group (ledger->book);
  if (group == NULL)
    return;

  cell = (ComboCell *)
    gnc_table_layout_get_cell (ledger->table->layout, ENTRY_ACCT_CELL);
  gnc_combo_cell_clear_menu (cell);
  load_xfer_cell (cell, group);

  cell = (ComboCell *)
    gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAXACC_CELL);
  gnc_combo_cell_clear_menu (cell);
  load_xfer_cell (cell, group);
}

void gnc_entry_ledger_load_xfer_cells (GncEntryLedger *ledger)
{
  load_xfer_type_cells (ledger);
}

/* XXX (FIXME): This should be in a config file! */
/* Copy GncEntry information from the list to the rows of the Ledger. */
void gnc_entry_ledger_load (GncEntryLedger *ledger, GList *entry_list)
{
  GncEntry *blank_entry;
  Table *table;

  GList *node;
  CellBlock *cursor_header, *cursor;
  VirtualCellLocation vcell_loc;
  VirtualLocation save_loc;
  time_t present;
  gboolean start_primary_color = TRUE;

  if (!ledger) return;

  /* Load up cells */
  load_discount_type_cells (ledger);
  load_tax_type_cells (ledger);
  load_inv_type_cells (ledger);
  load_xfer_type_cells (ledger);

  blank_entry = gncEntryLookup (ledger->book, &(ledger->blank_entry_guid));

  if (blank_entry == NULL) {
    switch (ledger->type) {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_INVOICE_ENTRY:
      blank_entry = gncEntryCreate (ledger->book);
      gncEntrySetDate (blank_entry, &ledger->last_date_entered);
      ledger->blank_entry_guid = *gncEntryGetGUID (blank_entry);
      break;
    default:
      ledger->blank_entry_guid = *xaccGUIDNULL ();
      break;
    }
    ledger->blank_entry_edited = FALSE;
  }

  table = ledger->table;

  gnc_table_leave_update (table, table->current_cursor_loc);
  save_loc = table->current_cursor_loc;

  /* disable move callback -- we don't want the cascade of 
   * callbacks while we are fiddling with loading the register */
  gnc_table_control_allow_move (table->control, FALSE);

  /* invalidate the cursor */
  {
    VirtualLocation virt_loc;

    virt_loc.vcell_loc.virt_row = -1;
    virt_loc.vcell_loc.virt_col = -1;
    virt_loc.phys_row_offset = -1;
    virt_loc.phys_col_offset = -1;

    gnc_table_move_cursor_gui (table, virt_loc);
  }

  /* make sure that the header is loaded */
  vcell_loc.virt_row = 0;
  vcell_loc.virt_col = 0;
  cursor_header = gnc_table_layout_get_cursor (table->layout, CURSOR_HEADER);
  gnc_table_set_vcell (table, cursor_header, NULL, TRUE, TRUE, vcell_loc);
  vcell_loc.virt_row++;

  /* get the current time and reset the dividing row */
  {
    struct tm *tm;

    present = time (NULL);

    tm = localtime (&present);
    tm->tm_sec = 59;
    tm->tm_min = 59;
    tm->tm_hour = 23;
    tm->tm_isdst = -1;

    present = mktime (tm);
  }

  table->model->dividing_row = -1;
  cursor = gnc_table_layout_get_cursor (table->layout, "cursor");

  /* Populate the table */
  for (node = entry_list; node; node = node->next) {
    GncEntry *entry = node->data;

    /* Don't load the blank entry */
    if (entry == blank_entry)
      continue;

    /* If this is the first load of the ledger, fill the quickfill cells */
    {
      /* XXX */
    }

    gnc_table_set_vcell (table, cursor, gncEntryGetGUID (entry),
			 TRUE, start_primary_color, vcell_loc);
    vcell_loc.virt_row++;

    /* Flip color for the next guy */
    start_primary_color = !start_primary_color;
  }

  /* Add the blank entry at the end. */
  if (blank_entry) {
    gnc_table_set_vcell (table, cursor, gncEntryGetGUID (blank_entry),
			 TRUE, start_primary_color, vcell_loc);
    vcell_loc.virt_row++;
  }

  /* Resize the table */
  gnc_table_set_size (table, vcell_loc.virt_row, 1);

  /* Restore the cursor */
  if (gnc_table_find_close_valid_cell (table, &save_loc, FALSE)) {
    gnc_table_move_cursor_gui (table, save_loc);

    /* restore_current_cursor()? */
  }

  /* Set the cell fractions */


  gnc_table_refresh_gui (table, TRUE);

  /* Show_entry ??? */

  /* Set completion character */

  /* enable callback for cursor user-driven moves */
  gnc_table_control_allow_move (table->control, TRUE);

  /* Set the confirmation callbacks and load the combo cells */
}

