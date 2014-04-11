/*
 * gncEntryLedgerControl.c -- Control for GncEntry ledger
 * Copyright (C) 2001, 2002, 2003 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "combocell.h"
#include "dialog-account.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
#include "table-allgui.h"
#include "pricecell.h"
#include "dialog-tax-table.h"

#include "gncEntryLedgerP.h"
#include "gncEntryLedgerControl.h"

static gboolean
gnc_entry_ledger_save (GncEntryLedger *ledger, gboolean do_commit)
{
  GncEntry *blank_entry;
  GncEntry *entry;

  if (!ledger) return FALSE;

  blank_entry = gnc_entry_ledger_get_blank_entry (ledger);

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

  if (!gncEntryIsOpen (entry))
    gncEntryBeginEdit (entry);

  gnc_table_save_cells (ledger->table, entry);

  if (entry == blank_entry) {
    Timespec ts;
    ts.tv_sec = time(NULL);
    ts.tv_nsec = 0;
    gncEntrySetDateEntered (blank_entry, ts);
    
    switch (ledger->type) {
    case GNCENTRY_ORDER_ENTRY:
      gncOrderAddEntry (ledger->order, blank_entry);
      break;
    case GNCENTRY_INVOICE_ENTRY:
      /* Anything entered on an invoice entry must be part of the invoice! */
      gncInvoiceAddEntry (ledger->invoice, blank_entry);
      break;
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_EXPVOUCHER_ENTRY:
      /* Anything entered on an invoice entry must be part of the invoice! */
      gncBillAddEntry (ledger->invoice, blank_entry);
      break;
    default:
      /* Nothing to do for viewers */
      g_warning ("blank entry traversed in a viewer");
      break;
    }
  }

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

static gboolean
gnc_entry_ledger_verify_acc_cell_ok (GncEntryLedger *ledger,
				     const char *cell_name,
				     const char *cell_msg)
{
  ComboCell *cell;
  const char *name;

  cell = (ComboCell *) gnc_table_layout_get_cell (ledger->table->layout,
						  cell_name);
  g_return_val_if_fail (cell, TRUE);
  name = cell->cell.value;
  if (!name || *name == '\0') {
    /* Translators: %s is the string "an Account" i.e. its translation. */
    const char *format = _("Invalid Entry:  You need to supply %s.");

    gnc_error_dialog (ledger->parent, format, cell_msg);
    return FALSE;
  }
  return TRUE;
}

/* Verify whether we can save the entry, or warn the user when we can't
 * return TRUE if we can save, FALSE if there is a problem
 */
static gboolean
gnc_entry_ledger_verify_can_save (GncEntryLedger *ledger)
{
  gnc_numeric value;

  /* Compute the value and tax value of the current cursor */
  gnc_entry_ledger_compute_value (ledger, &value, NULL);

  /* If there is a value, make sure there is an account */
  if (! gnc_numeric_zero_p (value)) {
    switch (ledger->type) {
    case GNCENTRY_INVOICE_ENTRY:
      if (!gnc_entry_ledger_verify_acc_cell_ok (ledger, ENTRY_IACCT_CELL,
						_("an Account")))
	return FALSE;
      break;
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_EXPVOUCHER_ENTRY:
      if (!gnc_entry_ledger_verify_acc_cell_ok (ledger, ENTRY_BACCT_CELL,
						_("an Account")))
	return FALSE;
      break;
    default:
      g_warning ("Unhandled ledger type");
      break;
    }
  }

  return TRUE;
}

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
  gnc_resume_gui_refresh ();

  /* redrawing can muck everything up */
  if (saved) {
    VirtualCellLocation vcell_loc;

    /* redraw */
    gnc_entry_ledger_display_refresh (ledger);

    if (ledger->traverse_to_new)
      new_entry = gnc_entry_ledger_get_blank_entry (ledger);

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

static gboolean
gnc_entry_ledger_auto_completion (GncEntryLedger *ledger,
				  gncTableTraversalDir dir,
				  VirtualLocation *p_new_virt_loc)
{
  GncEntry *entry;
  GncEntry *blank_entry;

  blank_entry = gnc_entry_ledger_get_blank_entry (ledger);

  /* auto-completion is only triggered by a tab out */
  if (dir != GNC_TABLE_TRAVERSE_RIGHT)
    return FALSE;

  entry = gnc_entry_ledger_get_current_entry (ledger);
  if (entry == NULL)
    return FALSE;

  switch (ledger->type) {
  case GNCENTRY_ORDER_ENTRY:
  case GNCENTRY_INVOICE_ENTRY:
  case GNCENTRY_BILL_ENTRY:
  case GNCENTRY_EXPVOUCHER_ENTRY:

    /* There must be a blank entry */
    if (blank_entry == NULL)
      return FALSE;

    /* we must be on the blank entry */
    if (entry != blank_entry)
      return FALSE;

    /* XXX: No other autocompletion, yet */
    break;

  default:
    break;
  }

  return TRUE;
}

static gboolean gnc_entry_ledger_traverse (VirtualLocation *p_new_virt_loc,
					   gncTableTraversalDir dir,
					   gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry, *new_entry;
  gint response;
  VirtualLocation virt_loc;
  int changed;
  char const *cell_name;
  gboolean exact_traversal;

  if (!ledger) return FALSE;

  exact_traversal = (dir == GNC_TABLE_TRAVERSE_POINTER);

  entry = gnc_entry_ledger_get_current_entry (ledger);
  if (!entry)
    return FALSE;

  /* no changes, make sure we aren't going off the end */
  changed = gnc_table_current_cursor_changed (ledger->table, FALSE);
  if (!changed)
    return FALSE;

  virt_loc = *p_new_virt_loc;

  cell_name = gnc_table_get_current_cell_name (ledger->table);

  /* See if we are leaving the account field */
  do
  {
    ComboCell *cell;
    char *name;
    char *cell_name = NULL;

    switch (ledger->type) {
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:
      cell_name = ENTRY_IACCT_CELL;
      break;
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_BILL_VIEWER:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_EXPVOUCHER_VIEWER:
      cell_name = ENTRY_BACCT_CELL;
      break;
    default:
      g_warning ("Unhandled ledger type");
      break;
    }

    if (!cell_name)
      break;

    if (!gnc_cell_name_equal (cell_name, cell_name))
      break;

    if (!gnc_table_layout_get_cell_changed (ledger->table->layout,
					    cell_name, FALSE))
      break;

    cell = (ComboCell *) gnc_table_layout_get_cell (ledger->table->layout,
						    cell_name);
    if (!cell)
      break;

    name = cell->cell.value;
    if (!name || *name == '\0')
      break;

    /* Create the account if necessary. Also checks for a placeholder */
    (void) gnc_entry_ledger_get_account_by_name (ledger, (BasicCell *)cell, cell->cell.value,
						 &ledger->full_refresh);

  } while (FALSE);


  /* See if we are leaving the TaxTable field */
  do
  {
    ComboCell *cell;
    GncTaxTable *table;
    char *name;

    if (!gnc_cell_name_equal (cell_name, ENTRY_TAXTABLE_CELL))
      break;

    if (!gnc_table_layout_get_cell_changed (ledger->table->layout,
					    ENTRY_TAXTABLE_CELL, FALSE))
      break;

    cell = (ComboCell *) gnc_table_layout_get_cell (ledger->table->layout,
						    ENTRY_TAXTABLE_CELL);
    if (!cell)
      break;

    name = cell->cell.value;
    if (!name || *name == '\0')
      break;

    table = gncTaxTableLookupByName (ledger->book, cell->cell.value);
    if (table)
      break;

    {
      const char *format = _("The tax table %s does not exist. "
                             "Would you like to create it?");
      if (!gnc_verify_dialog (ledger->parent, TRUE, format, name))
        break;
    }

    ledger->full_refresh = FALSE;

    table = gnc_ui_tax_table_new_from_name (ledger->book, name);
    if (!table)
      break;

    ledger->full_refresh = TRUE;

    name = (char *)gncTaxTableGetName (table);
    gnc_combo_cell_set_value (cell, name);
    gnc_basic_cell_set_changed (&cell->cell, TRUE);

  } while (FALSE);


  /* See if we are tabbing off the end of the very last line
   * (i.e. the blank entry)
   */
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

    /* Yep, we're trying to leave the blank entry -- make sure
     * we are allowed to do so by verifying the current cursor.
     * If the current cursor is ok, then move on!
     */

    /* Verify that the cursor is ok.  If we can't save the cell, don't move! */
    if (!gnc_entry_ledger_verify_can_save (ledger)) {
      return TRUE;
    }

    (p_new_virt_loc->vcell_loc.virt_row)++;
    p_new_virt_loc->phys_row_offset = 0;
    p_new_virt_loc->phys_col_offset = 0;

    ledger->traverse_to_new = TRUE;

    /* If we're here, we're tabbing off the end of the 'blank entry' */
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

  /* Check for going off the end */
  gnc_table_find_close_valid_cell (ledger->table, &virt_loc, exact_traversal);

  /* Same entry, no problem -- we're just moving backwards in the cursor */
  new_entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  if (entry == new_entry)
  {
    *p_new_virt_loc = virt_loc;

    return FALSE;
  }

  /* If we are here, then we are trying to leave the cursor.  Make sure
   * the cursor we are leaving is valid.  If so, ask the user if the
   * changes should be recorded.  If not, don't go anywhere.
   */

  /* Verify this cursor -- if it's not valid, don't let them move on */
  if (!gnc_entry_ledger_verify_can_save (ledger)) {
    *p_new_virt_loc = ledger->table->current_cursor_loc;
    return TRUE;
  }

  /*
   * XXX  GNCENTRY_INVOICE_EDIT processing to be added:
   * 1) check if the qty field changed.
   * 2) if so, check if this entry is part of an order.
   * 3) if so, ask if they want to change the entry or
   *    split the entry into two parts.
   */

  /* Ok, we are changing lines and the current entry has
   * changed. We only ask them what they want to do in
   * limited cases -- usually just let the change go through.
   */
  {
    GtkWidget *dialog;
    const char *title = _("Save the current entry?");
    const char *message =
      _("The current entry has been changed.  However, this entry is "
	"part of an existing order. Would you like to record the change "
	"and effectively change your order?");

    switch (ledger->type) {
    case GNCENTRY_INVOICE_ENTRY:
      if (gncEntryGetOrder (entry) != NULL) {
	dialog = gtk_message_dialog_new(GTK_WINDOW(ledger->parent),
					GTK_DIALOG_DESTROY_WITH_PARENT,
					GTK_MESSAGE_QUESTION,
					GTK_BUTTONS_NONE,
					"%s", title);
	gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
						 "%s", message);
	gtk_dialog_add_buttons(GTK_DIALOG(dialog),
			       _("_Don't Record"), GTK_RESPONSE_REJECT,
			       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			       _("_Record"), GTK_RESPONSE_ACCEPT,
			       NULL);
	response = gnc_dialog_run(GTK_DIALOG(dialog), "invoice_entry_changed");
	gtk_widget_destroy(dialog);
	break;
      }

      /* FALLTHROUGH */
    default:
      response = GTK_RESPONSE_ACCEPT;
      break;
    }
  }

  switch (response)
  {
    case GTK_RESPONSE_ACCEPT:
      break;

    case GTK_RESPONSE_REJECT:
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

    case GTK_RESPONSE_CANCEL:
    default:
      return TRUE;
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


void gnc_entry_ledger_cancel_cursor_changes (GncEntryLedger *ledger)
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
gnc_entry_ledger_check_close_internal (GtkWidget *parent,
				       GncEntryLedger *ledger,
				       gboolean dontask)
{
  const char *message = _("The current entry has been changed. "
			  "Would you like to save it?");
  VirtualLocation virt_loc;
  
  virt_loc = ledger->table->current_cursor_loc;
    
  if (gnc_entry_ledger_traverse (&virt_loc, GNC_TABLE_TRAVERSE_POINTER,
				 ledger))
    return FALSE;

  if (!gnc_entry_ledger_verify_can_save (ledger))
    return FALSE;

  if (dontask || gnc_verify_dialog (parent, TRUE, "%s", message))
    gnc_entry_ledger_save (ledger, TRUE);
  else
    gnc_entry_ledger_cancel_cursor_changes (ledger);

  return TRUE;
}

gboolean
gnc_entry_ledger_commit_entry (GncEntryLedger *ledger)
{
  if (!ledger) return TRUE;

  return gnc_entry_ledger_check_close_internal (NULL, ledger, TRUE);
}

gboolean
gnc_entry_ledger_check_close (GtkWidget *parent, GncEntryLedger *ledger)
{
  if (!ledger) return TRUE;

  if (gnc_entry_ledger_changed (ledger)) {
    gboolean dontask = FALSE;

    if (ledger->type ==  GNCENTRY_INVOICE_ENTRY) {
      gboolean inv_value;
      gboolean only_inv_changed = FALSE;

      if (gnc_table_current_cursor_changed (ledger->table, FALSE) == 1 &&
	  gnc_table_layout_get_cell_changed (ledger->table->layout,
					     ENTRY_INV_CELL, TRUE))
	only_inv_changed = TRUE;

      inv_value = gnc_entry_ledger_get_checkmark (ledger, ENTRY_INV_CELL);
      
      if (inv_value && only_inv_changed) {
	/* If the only change is that the 'inv' entry was clicked
	 * "on", then just accept the change it without question.
	 */
	dontask = TRUE;
      }
    }

    return gnc_entry_ledger_check_close_internal (parent, ledger, dontask);

  }
  return TRUE;
}
