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
#include "global-options.h"

#include "gncEntry.h"
#include "gncEntryLedger.h"
#include "gncEntryLedgerP.h"


/* XXX: This should go elsewhere */
const char * gnc_entry_ledger_type_string_getter (char flag)
{
  switch (flag) {
  case '1': return _("$");
  case '2': return _("%");
  default:
    return "?";
  };
}

const char * gnc_entry_ledger_how_string_getter (char flag)
{
  switch (flag) {
  case '1': return _("<");
  case '2': return _("=");
  case '3': return _(">");
  default:
    return "?";
  };
}

static void load_discount_type_cells (GncEntryLedger *ledger)
{
  RecnCell *cell;

  if (!ledger) return;

  cell = (RecnCell *)
    gnc_table_layout_get_cell (ledger->table->layout, ENTRY_DISTYPE_CELL);

  if (!cell) return;

  gnc_recn_cell_set_valid_flags (cell, "12", '2');
  gnc_recn_cell_set_flag_order (cell, "21");
  gnc_recn_cell_set_string_getter (cell, gnc_entry_ledger_type_string_getter);
}

static void load_discount_how_cells (GncEntryLedger *ledger)
{
  RecnCell *cell;

  if (!ledger) return;

  cell = (RecnCell *)
    gnc_table_layout_get_cell (ledger->table->layout, ENTRY_DISHOW_CELL);

  if (!cell) return;

  gnc_recn_cell_set_valid_flags (cell, "123", '1');
  gnc_recn_cell_set_flag_order (cell, "123");
  gnc_recn_cell_set_string_getter (cell, gnc_entry_ledger_how_string_getter);
}

static void load_inv_type_cells (GncEntryLedger *ledger, char *cell_name,
				 gboolean default_is_true)
{
  RecnCell *cell;

  if (!ledger) return;

  cell = (RecnCell *)
    gnc_table_layout_get_cell (ledger->table->layout, cell_name);

  if (!cell) return;

  gnc_recn_cell_set_valid_flags (cell, " X", (default_is_true ? 'X' : ' '));
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
}

static void load_taxtable_type_cells (GncEntryLedger *ledger)
{
  GList *list;
  ComboCell *cell;

  cell = (ComboCell *)
    gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAXTABLE_CELL);
  gnc_combo_cell_clear_menu (cell);
  
  list = gncTaxTableGetTables (ledger->book);
  for ( ; list ; list = list->next ) {
    GncTaxTable *table = list->data;
    const char *name = gncTaxTableGetName (table);
    if (name != NULL)
      gnc_combo_cell_add_menu_item (cell, (char*)name);
  }
}

void gnc_entry_ledger_load_xfer_cells (GncEntryLedger *ledger)
{
  load_xfer_type_cells (ledger);
  load_taxtable_type_cells (ledger);
}

/* XXX (FIXME): This should be in a config file! */
/* Copy GncEntry information from the list to the rows of the Ledger. */
void gnc_entry_ledger_load (GncEntryLedger *ledger, GList *entry_list)
{
  GncEntry *blank_entry, *find_entry;
  CursorBuffer *cursor_buffer;
  Table *table;

  GList *node;
  CellBlock *cursor_header, *cursor;
  VirtualCellLocation vcell_loc;
  VirtualLocation save_loc;
  time_t present;
  gboolean start_primary_color = TRUE;

  int new_entry_row = -1;

  if (!ledger) return;

  /* Load up cells */
  load_discount_type_cells (ledger);
  load_discount_how_cells (ledger);
  load_inv_type_cells (ledger, ENTRY_INV_CELL, FALSE);
  load_inv_type_cells (ledger, ENTRY_TAXABLE_CELL, TRUE);
  load_inv_type_cells (ledger, ENTRY_TAXINCLUDED_CELL, FALSE);
  gnc_entry_ledger_load_xfer_cells (ledger);

  blank_entry = gnc_entry_ledger_get_blank_entry (ledger);

  if (blank_entry == NULL) {
    switch (ledger->type) {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_INVOICE_ENTRY:
      blank_entry = gncEntryCreate (ledger->book);
      gncEntrySetDate (blank_entry, ledger->last_date_entered);
      ledger->blank_entry_guid = *gncEntryGetGUID (blank_entry);

      if (ledger->type == GNCENTRY_INVOICE_ENTRY) {
	GncOwner *owner = gncInvoiceGetOwner (ledger->invoice);
	GncTaxTable *table = NULL;
	GncTaxIncluded taxincluded_p = GNC_TAXINCLUDED_USEGLOBAL;
	gboolean taxincluded;
	gnc_numeric discount = gnc_numeric_zero ();

	owner = gncOwnerGetEndOwner (owner);
	switch (gncOwnerGetType (owner)) {
	case GNC_OWNER_CUSTOMER:
	  taxincluded_p = gncCustomerGetTaxIncluded (owner->owner.customer);
	  discount = gncCustomerGetDiscount (owner->owner.customer);
	  break;
	case GNC_OWNER_VENDOR:
	  taxincluded_p = gncVendorGetTaxIncluded (owner->owner.vendor);
	  break;
	default:
	}

	/* XXX: Get the default tax-table */

	/* Compute the default taxincluded */
	switch (taxincluded_p) {
	case GNC_TAXINCLUDED_YES:
	  taxincluded = TRUE;
	  break;
	case GNC_TAXINCLUDED_NO:
	  taxincluded = FALSE;
	  break;
	case GNC_TAXINCLUDED_USEGLOBAL:
	  taxincluded = gnc_lookup_boolean_option ("Business",
						   "Tax Included?", FALSE);
	  break;
	}

	gncEntrySetTaxTable (blank_entry, table);
	gncEntrySetTaxIncluded (blank_entry, taxincluded);
	gncEntrySetDiscount (blank_entry, discount);
      }
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

  /* Figure out where we are going to */
  if (ledger->traverse_to_new) {
    find_entry = blank_entry;
  } else if (ledger->hint_entry) {
    find_entry = ledger->hint_entry;
  } else {
    find_entry = gnc_entry_ledger_get_current_entry(ledger);
		/* XXX: get current entry (cursor_hint_xxx) */
  }

  /* If the current cursor has changed we save the values for later
   * possible restoration. */
  if (gnc_table_current_cursor_changed (table, TRUE) &&
      (find_entry == gnc_entry_ledger_get_current_entry (ledger)))
  {
    cursor_buffer = gnc_cursor_buffer_new ();
    gnc_table_save_current_cursor (table, cursor_buffer);
  }
  else
    cursor_buffer = NULL;

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

    if (entry == find_entry)
      new_entry_row = vcell_loc.virt_row;

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

    if (find_entry == blank_entry)
      new_entry_row = vcell_loc.virt_row;

    vcell_loc.virt_row++;
  }

  /* Resize the table */
  gnc_table_set_size (table, vcell_loc.virt_row, 1);

  /* Restore the cursor to its rightful position */
  if (new_entry_row > 0)
    save_loc.vcell_loc.virt_row = new_entry_row;

  if (gnc_table_find_close_valid_cell (table, &save_loc, FALSE)) {
    gnc_table_move_cursor_gui (table, save_loc);

    if (find_entry == gnc_entry_ledger_get_current_entry (ledger))
      gnc_table_restore_current_cursor (table, cursor_buffer);
  }

  gnc_cursor_buffer_destroy (cursor_buffer);
  cursor_buffer = NULL;

  /* Reset the ledger */
  ledger->traverse_to_new = FALSE;
  ledger->hint_entry = NULL;

  /* Set the cell fractions */


  gnc_table_refresh_gui (table, TRUE);

  /* Show_entry ??? */

  /* Set completion character */

  /* enable callback for cursor user-driven moves */
  gnc_table_control_allow_move (table->control, TRUE);

  /* Set the confirmation callbacks and load the combo cells */
}

