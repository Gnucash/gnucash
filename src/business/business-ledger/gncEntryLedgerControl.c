/*
 * gncEntryLedgerControl.c -- Control for GncEntry ledger
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#define _GNU_SOURCE

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "AccWindow.h"
#include "combocell.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "messages.h"
#include "table-allgui.h"
#include "pricecell.h"

#include "gncEntryLedgerP.h"
#include "gncEntryLedgerControl.h"

static void gnc_entry_ledger_move_cursor (VirtualLocation *p_new_virt_loc,
					  gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  VirtualLocation new_virt_loc = *p_new_virt_loc;
  GncEntry *new_entry;
  GncEntry *old_entry;
  gboolean saved;

  if (!ledger) return;

  old_entry = gnc_entry_ledger_get_current_entry (ledger);
  new_entry = gnc_entry_ledger_get_entry (ledger, new_virt_loc.vcell_loc);

  gnc_suspend_gui_refresh ();
  saved = gnc_entry_ledger_save (ledger, old_entry != new_entry);

  gnc_resume_gui_refresh();

  /* redrawing can muck everything up */
  if (saved) {
    VirtualCellLocation vcell_loc;

    /* redraw */
    gnc_entry_ledger_redraw (ledger);

    if (ledger->traverse_to_new)
      new_entry = gncEntryLookup (ledger->book, &ledger->blank_entry_guid);

    /* if the entry we were going to is still in the register,
     * then it may have moved. Find out where it is now. */
    if (gnc_entry_ledger_find_entry (ledger, new_entry, &vcell_loc)) {
      VirtualCell *vcell;

      vcell = gnc_table_get_virtual_cell (ledger->table, vcell_loc);
      new_virt_loc.vcell_loc = vcell_loc;
    } else
      new_virt_loc.vcell_loc = ledger->table->current_cursor_loc.vcell_loc;
  }

  gnc_table_find_close_valid_cell (ledger->table, &new_virt_loc, FALSE);

  *p_new_virt_loc = new_virt_loc;
}

static void gnc_entry_ledger_cancel_cursor_changes (GncEntryLedger *ledger)
{
  VirtualLocation virt_loc;

  if (ledger == NULL)
    return;

  virt_loc = ledger->table->current_cursor_loc;

  if (!gnc_table_current_cursor_changed (ledger->table, FALSE))
    return;

  /* When cancelling edits, reload the cursor from the entry. */
  gnc_table_clear_current_cursor_changes (ledger->table);

  if (gnc_table_find_close_valid_cell (ledger->table, &virt_loc, FALSE))
    gnc_table_move_cursor_gui (ledger->table, virt_loc);

  gnc_table_refresh_gui (ledger->table, TRUE);
}

static gboolean
gnc_entry_ledger_auto_completion (GncEntryLedger *ledger,
				  gncTableTraversalDir dir,
				  VirtualLocation *p_new_virt_loc)
{
  GncEntry *entry;
  GncEntry *blank_entry;

  blank_entry = gncEntryLookup (ledger->book, &ledger->blank_entry_guid);

  /* auto-completion is only triggered by a tab out */
  if (dir != GNC_TABLE_TRAVERSE_RIGHT)
    return FALSE;

  entry = gnc_entry_ledger_get_current_entry (ledger);
  if (entry == NULL)
    return FALSE;

  /* No other autocompletion, yet */

  return TRUE;
}

static gboolean gnc_entry_ledger_traverse (VirtualLocation *p_new_virt_loc,
					   gncTableTraversalDir dir,
					   gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry, *new_entry;
  GNCVerifyResult result;
  VirtualLocation virt_loc;
  gboolean changed;
  char const *cell_name;
  gboolean exact_traversal;

  if (!ledger) return FALSE;

  exact_traversal = (dir == GNC_TABLE_TRAVERSE_POINTER);

  ledger->traverse_to_new = FALSE;

  entry = gnc_entry_ledger_get_current_entry (ledger);
  if (!entry)
    return FALSE;

  /* no changes, make sure we aren't going off the end */
  changed = gnc_table_current_cursor_changed (ledger->table, FALSE);
  if (!changed)
    return FALSE;

  virt_loc = *p_new_virt_loc;

  cell_name = gnc_table_get_current_cell_name (ledger->table);

  /* See if we are leaving an account field */
  do
  {
    ComboCell *cell;
    Account *account;
    char *name;

    if (!gnc_cell_name_equal (cell_name, ENTRY_ACCT_CELL) &&
        !gnc_cell_name_equal (cell_name, ENTRY_TAXACC_CELL))
      break;

    cell = NULL;

    if (gnc_cell_name_equal (cell_name, ENTRY_ACCT_CELL))
    {
      if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                             ENTRY_ACCT_CELL, FALSE))
        cell = (ComboCell *) gnc_table_layout_get_cell (ledger->table->layout,
                                                        ENTRY_ACCT_CELL);
    }

    if (gnc_cell_name_equal (cell_name, ENTRY_TAXACC_CELL))
    {
      if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                             ENTRY_TAXACC_CELL, FALSE))
        cell = (ComboCell *) gnc_table_layout_get_cell (ledger->table->layout,
                                                        ENTRY_TAXACC_CELL);
    }

    if (!cell)
      break;

    name = cell->cell.value;
    if (!name || *name == '\0')
      break;

    account = xaccGetAccountFromFullName (gnc_get_current_group (),
                                          cell->cell.value,
                                          gnc_get_account_separator ());
    if (account)
      break;

    {
      const char *format = _("The account %s does not exist.\n"
                             "Would you like to create it?");
      char *message;
      gboolean result;

      message = g_strdup_printf (format, name);

      result =
	gnc_verify_dialog_parented (ledger->parent, message, TRUE);
      if (!result)
        break;
    }

    //    info->full_refresh = FALSE;

    account = gnc_ui_new_accounts_from_name_window (name);
    if (!account)
      break;

    // info->full_refresh = TRUE;

    name = xaccAccountGetFullName (account, gnc_get_account_separator ());
    gnc_combo_cell_set_value (cell, name);
    gnc_basic_cell_set_changed (&cell->cell, TRUE);
    g_free (name);

  } while (FALSE);

  /* 
   * XXX: Note well that there is no verification that the Entry has
   * an account or taxaccount, which could cause trouble come invoice
   * time.  Should we check that here?  Note that we only need an
   * account or taxaccount if there is a value or tax.  E.g., if the
   * tax is zero, then we don't need a taxaccount.
   */

  /* See if we are tabbing off the end of the very last line */
  do
  {
    VirtualLocation virt_loc;

    if (!changed && !ledger->blank_entry_edited)
      break;

    if (dir != GNC_TABLE_TRAVERSE_RIGHT)
      break;

    virt_loc = ledger->table->current_cursor_loc;
    if (gnc_table_move_vertical_position (ledger->table, &virt_loc, 1))
      break;

    virt_loc = ledger->table->current_cursor_loc;
    if (gnc_table_move_tab (ledger->table, &virt_loc, TRUE))
      break;

    *p_new_virt_loc = ledger->table->current_cursor_loc;
    (p_new_virt_loc->vcell_loc.virt_row)++;
    p_new_virt_loc->phys_row_offset = 0;
    p_new_virt_loc->phys_col_offset = 0;

    ledger->traverse_to_new = TRUE;

    return FALSE;

  } while (FALSE);

  /* Now see if we are changing cursors. If not, we may be able to
   * auto-complete. */
  if (!gnc_table_virtual_cell_out_of_bounds (ledger->table,
					     virt_loc.vcell_loc))
  {
    if (gnc_entry_ledger_auto_completion (ledger, dir, p_new_virt_loc))
      return FALSE;
  }

  /* See if we are tabbing off the end of a blank entry */
  do
  {
    VirtualLocation virt_loc;
    int old_virt_row;

    if (!changed)
      break;

    if (dir != GNC_TABLE_TRAVERSE_RIGHT)
      break;

    virt_loc = ledger->table->current_cursor_loc;
    old_virt_row = virt_loc.vcell_loc.virt_row;

    if (gnc_table_move_tab (ledger->table, &virt_loc, TRUE) &&
        old_virt_row == virt_loc.vcell_loc.virt_row)
      break;

    return FALSE;

  } while (FALSE);

  /* Check for going off the end */
  gnc_table_find_close_valid_cell (ledger->table, &virt_loc, exact_traversal);

  /* Same transaction, no problem */
  new_entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  if (entry == new_entry)
  {
    *p_new_virt_loc = virt_loc;

    return FALSE;
  }

  /*
   * XXX  GNCENTRY_INVOICE_EDIT processing to be added:
   * 1) check if the qty field changed.
   * 2) if so, check if this entry is part of an order.
   * 3) if so, ask if they want to change the entry or
   *    split the entry into two parts.
   */

  /* Ok, we are changing entries and the current entry has
   * changed. See what the user wants to do. */
  {
    const char *message;

    switch (ledger->type) {
    case GNCENTRY_INVOICE_ENTRY:
      {
	char inv_value;
	char only_inv_changed;

	inv_value = gnc_entry_ledger_get_inv (ledger, ENTRY_INV_CELL);
	only_inv_changed = 0; /* XXX */

	if (inv_value == 'X' && only_inv_changed) {
	  /* If the only change is that the 'inv' entry was clicked
	   * "on", then just accept the change it without question.
	   */
	  result = GNC_VERIFY_YES;
	  goto dontask;
	}
      }
      /* Ok, something else has changed -- we should ask the user */

      if (gncEntryGetOrder (entry) != NULL) {
	message = _("The current entry has been changed.\n"
		    "However, this entry is part of an existing order\n"
		    "Would you like to record the change and\n"
		    "effectively change your order?");
	break;
      }
      /* FALLTHROUGH */
    default:
      message = _("The current entry has been changed.\n"
		  "Would you like to record the change?");
      break;
    }

    result = gnc_verify_cancel_dialog_parented (ledger->parent,
						message, GNC_VERIFY_YES);
  }

dontask:

  switch (result)
  {
    case GNC_VERIFY_YES:
      break;

    case GNC_VERIFY_NO:
      {
        VirtualCellLocation vcell_loc;
	GncEntry *new_entry;

	new_entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

	gnc_entry_ledger_cancel_cursor_changes (ledger);

	if (gnc_entry_ledger_find_entry (ledger, new_entry, &vcell_loc))
	  virt_loc.vcell_loc = vcell_loc;

        gnc_table_find_close_valid_cell (ledger->table, &virt_loc,
                                         exact_traversal);

        *p_new_virt_loc = virt_loc;
      }

      break;

    case GNC_VERIFY_CANCEL:
      return TRUE;

    default:
      break;
  }

  return FALSE;
}

TableControl * gnc_entry_ledger_control_new (void)
{
  TableControl * control;

  control = gnc_table_control_new ();
  control->move_cursor = gnc_entry_ledger_move_cursor;
  control->traverse = gnc_entry_ledger_traverse;

  return control;
}

gboolean gnc_entry_ledger_save (GncEntryLedger *ledger, gboolean do_commit)
{
  GncEntry *blank_entry;
  GncEntry *entry;

  if (!ledger) return FALSE;

  blank_entry = gncEntryLookup (ledger->book, &(ledger->blank_entry_guid));

  entry = gnc_entry_ledger_get_current_entry (ledger);
  if (entry == NULL) return FALSE;

  /* Try to avoid heavy-weight updates if nothing has changed */
  if (!gnc_table_current_cursor_changed (ledger->table, FALSE)) {
    if (!do_commit) return FALSE;

    if (entry == blank_entry) {
      if (ledger->blank_entry_edited) {
	ledger->last_date_entered = gncEntryGetDate (entry);
	ledger->blank_entry_guid = *xaccGUIDNULL ();
	ledger->blank_entry_edited = FALSE;
	blank_entry = NULL;
      } else
	return FALSE;
    }

    return TRUE;
  }

  gnc_suspend_gui_refresh ();

  if (entry == blank_entry)
    switch (ledger->type) {
    case GNCENTRY_ORDER_ENTRY:
      gncOrderAddEntry (ledger->order, blank_entry);
      break;
    case GNCENTRY_INVOICE_ENTRY:
      /* Anything entered on an invoice entry must be part of the invoice! */
      gncInvoiceAddEntry (ledger->invoice, blank_entry);
      break;
    default:
      /* Nothing to do for viewers */
      break;
    }

  gnc_table_save_cells (ledger->table, entry);

  if (entry == blank_entry) {
    if (do_commit) {
      ledger->blank_entry_guid = *xaccGUIDNULL ();
      blank_entry = NULL;
      ledger->last_date_entered = gncEntryGetDate (entry);
    } else
      ledger->blank_entry_edited = TRUE;
  }

  if (do_commit)
    gncEntryCommitEdit (entry);

  gnc_table_clear_current_cursor_changes (ledger->table);

  gnc_resume_gui_refresh ();

  return TRUE;
}
