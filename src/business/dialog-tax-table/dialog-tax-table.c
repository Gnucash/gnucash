/*
 * dialog-tax-table.c -- Dialog to create and edit tax-tables
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-amount-edit.h"
#include "gnc-tree-view-account.h"

#include "gncTaxTable.h"
#include "dialog-tax-table.h"

#define DIALOG_TAX_TABLE_CM_CLASS "tax-table-dialog"
#define GCONF_SECTION "dialogs/business/tax_tables"

void tax_table_new_table_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_delete_table_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_new_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_edit_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_delete_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_row_selected (GtkCList *clist, gint row, gint column,
			     GdkEventButton *event, gpointer user_data);
void tax_table_entry_row_selected (GtkCList *clist, gint row, gint column,
				   GdkEventButton *event, gpointer user_data);
void tax_table_window_close (GtkWidget *widget, gpointer data);
void tax_table_window_destroy_cb (GtkWidget *widget, gpointer data);


struct _taxtable_window {
  GtkWidget *	dialog;
  GtkWidget *	names_clist;
  GtkWidget *	entries_clist;

  GncTaxTable *	current_table;
  GncTaxTableEntry *	current_entry;
  GNCBook *	book;
  gint		component_id;
};

typedef struct _new_taxtable {
  GtkWidget *	dialog;
  GtkWidget *	name_entry;
  GtkWidget *	amount_entry;
  GtkWidget *	acct_tree;

  GncTaxTable *		created_table;
  TaxTableWindow *	ttw;
  GncTaxTableEntry *	entry;
  gint			type;
  gboolean		new_table;
} NewTaxTable;


static gboolean
new_tax_table_ok_cb (NewTaxTable *ntt)
{
  TaxTableWindow *ttw;
  const char *name = NULL;
  char *message;
  Account *acc;
  gnc_numeric amount;

  g_return_val_if_fail (ntt, FALSE);
  ttw = ntt->ttw;

  /* Verify that we've got real, valid data */

  /* verify the name, maybe */
  if (ntt->new_table) {
    name = gtk_entry_get_text (GTK_ENTRY (ntt->name_entry));
    if (name == NULL || *name == '\0') {
      message = _("You must provide a name for this Tax Table.");
      gnc_error_dialog (ntt->dialog, message);
      return FALSE;
    }
    if (gncTaxTableLookupByName (ttw->book, name)) {
      message = g_strdup_printf(_(
			 "You must provide a unique name for this Tax Table.\n"
			 "Your choice \"%s\" is already in use."), name);
      gnc_error_dialog (ntt->dialog, "%s", message);
      g_free (message);
      return FALSE;
    }
  }

  /* verify the amount */
  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (ntt->amount_entry));
  if (gnc_numeric_negative_p (amount)) {
    message = _("Negative amounts are not allowed.");
    gnc_error_dialog (ntt->dialog, message);
    return FALSE;
  }
  if (ntt->type == GNC_AMT_TYPE_PERCENT &&
      gnc_numeric_compare (amount,
			   gnc_numeric_create (100, 1)) > 0) {
    message = _("Percentage amount must be between 0 and 100.");
    gnc_error_dialog (ntt->dialog, message);
    return FALSE;
  }							   

  /* verify the account */
  acc = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(ntt->acct_tree));
  if (acc == NULL) {
    message = _("You must choose a Tax Account.");
    gnc_error_dialog (ntt->dialog, message);
    return FALSE;
  }

  gnc_suspend_gui_refresh ();

  /* Ok, it's all valid, now either change to add this thing */
  if (ntt->new_table) {
    GncTaxTable *table = gncTaxTableCreate (ttw->book);
    gncTaxTableBeginEdit (table);
    gncTaxTableSetName (table, name);
    /* Reset the current table */
    ttw->current_table = table;
    ntt->created_table = table;
  } else
    gncTaxTableBeginEdit (ttw->current_table);

  /* Create/edit the entry */
  {
    GncTaxTableEntry *entry;

    if (ntt->entry) {
      entry = ntt->entry;
    } else {
      entry = gncTaxTableEntryCreate ();
      gncTaxTableAddEntry (ttw->current_table, entry);
      ttw->current_entry = entry;
    }

    gncTaxTableEntrySetAccount (entry, acc);
    gncTaxTableEntrySetType (entry, ntt->type);
    gncTaxTableEntrySetAmount (entry, amount);
  }

  /* Mark the table as changed and commit it */
  gncTaxTableChanged (ttw->current_table);
  gncTaxTableCommitEdit (ttw->current_table);

  gnc_resume_gui_refresh();
  return TRUE;
}

static void
optionmenu_changed (GtkWidget *widget, NewTaxTable *ntt)
{
  g_return_if_fail (ntt);
  ntt->type = GPOINTER_TO_INT (g_object_get_data (G_OBJECT(widget), "option"));
}

static GtkWidget *
add_menu_item (GtkWidget *menu, NewTaxTable *ntt, char *label, gint type)
	       
{
  GtkWidget *item;

  item = gtk_menu_item_new_with_label (label);
  g_object_set_data (G_OBJECT (item), "option", GINT_TO_POINTER (type));
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (optionmenu_changed), ntt);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  gtk_widget_show (item);
  return item;
}

static GtkWidget *
make_menu (GtkWidget *omenu, NewTaxTable *ntt)
{
  GtkWidget *menu, *value, *percent;
  int current = ntt->type - 1;

  menu = gtk_menu_new ();
  value = add_menu_item (menu, ntt, _("Value $"), GNC_AMT_TYPE_VALUE);
  /* xgettext:no-c-format */
  percent = add_menu_item (menu, ntt, _("Percent %"), GNC_AMT_TYPE_PERCENT);

  gtk_option_menu_set_menu (GTK_OPTION_MENU (omenu), menu);

  g_signal_emit_by_name (G_OBJECT ((current == GNC_AMT_TYPE_VALUE-1 ?
					value : percent)), "activate", ntt);
  gtk_option_menu_set_history (GTK_OPTION_MENU (omenu), current);
  return menu;
}

static GncTaxTable *
new_tax_table_dialog (TaxTableWindow *ttw, gboolean new_table,
		      GncTaxTableEntry *entry, const char *name)
{
  GncTaxTable *created_table = NULL;
  NewTaxTable *ntt;
  GladeXML *xml;
  GtkWidget *box, *widget;
  gboolean done;
  gint response;

  if (!ttw) return NULL;
  if (new_table && entry) return NULL;

  ntt = g_new0 (NewTaxTable, 1);
  ntt->ttw = ttw;
  ntt->entry = entry;
  ntt->new_table = new_table;

  if (entry)
    ntt->type = gncTaxTableEntryGetType (entry);
  else
    ntt->type = GNC_AMT_TYPE_PERCENT;

  /* Open and read the XML */
  xml = gnc_glade_xml_new ("tax-tables.glade", "New Tax Table Dialog");
  ntt->dialog = glade_xml_get_widget (xml, "New Tax Table Dialog");
  ntt->name_entry = glade_xml_get_widget (xml, "name_entry");
  if (name)
    gtk_entry_set_text (GTK_ENTRY (ntt->name_entry), name);

  /* Create the menu */
  make_menu (glade_xml_get_widget (xml, "type_menu"), ntt);

  /* Attach our own widgets */
  box = glade_xml_get_widget (xml, "amount_box");
  ntt->amount_entry = widget = gnc_amount_edit_new ();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (widget), TRUE);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (widget), 100000);
  gtk_box_pack_start (GTK_BOX (box), widget, TRUE, TRUE, 0);

  box = glade_xml_get_widget (xml, "acct_window");
  ntt->acct_tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
  gtk_container_add (GTK_CONTAINER (box), ntt->acct_tree);
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(ntt->acct_tree), FALSE);
  gnc_tree_view_configure_columns (GNC_TREE_VIEW(ntt->acct_tree), NULL);

  /* Make 'enter' do the right thing */
  gtk_entry_set_activates_default(GTK_ENTRY (gnc_amount_edit_gtk_entry
					     (GNC_AMOUNT_EDIT (ntt->amount_entry))),
				  TRUE);

  /* Fix mnemonics for generated target widgets */
  widget = glade_xml_get_widget (xml, "value_label");
  gtk_label_set_mnemonic_widget (GTK_LABEL (widget), ntt->amount_entry);
  widget = glade_xml_get_widget (xml, "account_label");
  gtk_label_set_mnemonic_widget (GTK_LABEL (widget), ntt->acct_tree);

  /* Fill in the widgets appropriately */
  if (entry) {
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ntt->amount_entry),
				gncTaxTableEntryGetAmount (entry));
    gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT (ntt->acct_tree),
						gncTaxTableEntryGetAccount (entry));
  }

  /* Set our parent */
  gtk_window_set_transient_for (GTK_WINDOW(ntt->dialog), GTK_WINDOW(ttw->dialog));

  /* Setup signals */
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     ntt);

  /* Show what we should */
  gtk_widget_show_all (ntt->dialog);
  if (new_table == FALSE) {
    gtk_widget_hide (glade_xml_get_widget (xml, "table_title"));
    gtk_widget_hide (glade_xml_get_widget (xml, "table_name"));
    gtk_widget_hide (glade_xml_get_widget (xml, "spacer"));
    gtk_widget_hide (ntt->name_entry);
    /* Tables are great for layout, but a pain when you hide widgets */
    widget = glade_xml_get_widget (xml, "ttd_table");
    gtk_table_set_row_spacing (GTK_TABLE(widget), 0, 0);
    gtk_table_set_row_spacing (GTK_TABLE(widget), 1, 0);
    gtk_table_set_row_spacing (GTK_TABLE(widget), 2, 0);
    gtk_widget_grab_focus (gnc_amount_edit_gtk_entry
			   (GNC_AMOUNT_EDIT (ntt->amount_entry)));
  } else
    gtk_widget_grab_focus (ntt->name_entry);

  /* Display the dialog now that we're done manipulating it */
  gtk_widget_show (ntt->dialog);

  done = FALSE;
  while (!done) {
    response = gtk_dialog_run (GTK_DIALOG (ntt->dialog));
    switch (response) {
     case GTK_RESPONSE_OK:
      if (new_tax_table_ok_cb (ntt)) {
	created_table = ntt->created_table;
	done = TRUE;
      }
      break;
     default:
      done = TRUE;
      break;
    }
  }

  gtk_widget_destroy(ntt->dialog);
  g_free(ntt);

  return created_table;
}

/***********************************************************************/

static void
tax_table_entries_refresh (TaxTableWindow *ttw, gboolean new_table)
{
  GList *list, *node;
  GtkAdjustment *vadjustment = NULL;
  GtkCList *clist;
  gfloat save_value = 0.0;

  g_return_if_fail (ttw);

  clist = GTK_CLIST (ttw->entries_clist);

  if (!new_table) {
    vadjustment = gtk_clist_get_vadjustment (clist);
    if (vadjustment)
      save_value = vadjustment->value;
  }

  /* Clear the list */
  gtk_clist_freeze (clist);
  gtk_clist_clear (clist);

  /* Add the items to the list */
  list = gncTaxTableGetEntries (ttw->current_table);
  if (list)
    list = g_list_reverse (g_list_copy (list));

  for (node = list ; node; node = node->next) {
    char *row_text[3];
    gint row;
    GncTaxTableEntry *entry = node->data;
    Account *acc = gncTaxTableEntryGetAccount (entry);
    gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

    row_text[0] = xaccAccountGetFullName (acc, gnc_get_account_separator ());
    switch (gncTaxTableEntryGetType (entry)) {
    case GNC_AMT_TYPE_PERCENT:
     row_text[1] =
       g_strdup_printf ("%s%%",
			xaccPrintAmount (amount,
					 gnc_default_print_info (FALSE)));
     break;
    default:
      row_text[1] =
	g_strdup_printf ("%s",
			 xaccPrintAmount (amount,
					  gnc_default_print_info (TRUE)));
      break;
    }
    row_text[2] = NULL;

    row = gtk_clist_prepend (clist, row_text);
    gtk_clist_set_row_data (clist, row, entry);
    gtk_clist_set_selectable (clist, row, TRUE);

    g_free (row_text[0]);
    g_free (row_text[1]);
  }

  g_list_free (list);

  if (!new_table) {
    if (vadjustment) {
      save_value = CLAMP (save_value, vadjustment->lower,
			  vadjustment->upper - vadjustment->page_size);
      gtk_adjustment_set_value (vadjustment, save_value);
    }
  }

  gtk_clist_thaw (clist);

  {
    gint row = gtk_clist_find_row_from_data (clist, ttw->current_entry);

    if (row < 0)
      row = 0;

    gtk_clist_select_row (clist, row, 0);

    /* If this row isn't visible, move it to the center */
    if (gtk_clist_row_is_visible (clist, row) != GTK_VISIBILITY_FULL)
      gtk_clist_moveto (clist, row, 0, 0.5, 0);
  }
}

static void
tax_table_window_refresh (TaxTableWindow *ttw)
{
  GList *list, *node;
  GtkAdjustment *vadjustment;
  GtkCList *clist;
  gfloat save_value = 0.0;

  g_return_if_fail (ttw);
  clist = GTK_CLIST (ttw->names_clist);

  vadjustment = gtk_clist_get_vadjustment (clist);
  if (vadjustment)
    save_value = vadjustment->value;

  /* Clear the list */
  gtk_clist_freeze (clist);
  gtk_clist_clear (clist);

  gnc_gui_component_clear_watches (ttw->component_id);

  /* Add the items to the list */
  list = gncTaxTableGetTables (ttw->book);

  /* If there are no tables, clear the entries list */
  if (list == NULL)
    gtk_clist_clear (GTK_CLIST (ttw->entries_clist));
  else
    list = g_list_reverse (g_list_copy (list));

  for (node = list; node; node = node->next) {
    char *row_text[2];
    gint row;
    GncTaxTable *table = node->data;

    gnc_gui_component_watch_entity (ttw->component_id,
				    gncTaxTableGetGUID (table),
				    GNC_EVENT_MODIFY);

    row_text[0] = (char *)gncTaxTableGetName (table);
    row_text[1] = NULL;

    row = gtk_clist_prepend (clist, row_text);
    gtk_clist_set_row_data (clist, row, table);
    gtk_clist_set_selectable (clist, row, TRUE);
  }

  g_list_free (list);

  gnc_gui_component_watch_entity_type (ttw->component_id,
				       GNC_TAXTABLE_MODULE_NAME,
				       GNC_EVENT_CREATE | GNC_EVENT_DESTROY);

  if (vadjustment) {
    save_value = CLAMP (save_value, vadjustment->lower,
			vadjustment->upper - vadjustment->page_size);
    gtk_adjustment_set_value (vadjustment, save_value);
  }

  gtk_clist_thaw (clist);

  {
    gint row = gtk_clist_find_row_from_data (clist, ttw->current_table);

    if (row < 0)
      row = 0;

    gtk_clist_select_row (clist, row, 0);

    /* If this row isn't visible, move it to the center */
    if (gtk_clist_row_is_visible (clist, row) != GTK_VISIBILITY_FULL)
      gtk_clist_moveto (clist, row, 0, 0.5, 0);
  }
  /* select_row() above will refresh the entries window */
}

void
tax_table_row_selected (GtkCList *clist, gint row, gint column,
			GdkEventButton *event, gpointer user_data)
{
  TaxTableWindow *ttw = user_data;
  GncTaxTable *table = gtk_clist_get_row_data (clist, row);

  g_return_if_fail (ttw);
  g_return_if_fail (table);

  /* If we've changed, then reset the entry list */
  if (table != ttw->current_table) {
    ttw->current_table = table;
    ttw->current_entry = NULL;
  }
  /* And force a refresh of the entries */
  tax_table_entries_refresh (ttw, TRUE);
}

void
tax_table_entry_row_selected (GtkCList *clist, gint row, gint column,
			      GdkEventButton *event, gpointer user_data)
{
  TaxTableWindow *ttw = user_data;
  GncTaxTableEntry *entry = gtk_clist_get_row_data (clist, row);

  g_return_if_fail (ttw);
  g_return_if_fail (entry);

  ttw->current_entry = entry;

  /* If we double-click an item, then pop up an 'edit' window */
  if (event && event->type == GDK_2BUTTON_PRESS)
    new_tax_table_dialog (ttw, FALSE, entry, NULL);
}

void
tax_table_new_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);
  new_tax_table_dialog (ttw, TRUE, NULL, NULL);
}

void
tax_table_delete_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);

  if (!ttw->current_table)
    return;

  if (gncTaxTableGetRefcount (ttw->current_table) > 0) {
    char *message =
      g_strdup_printf (_("Tax table \"%s\" is in use.  You cannot delete it."),
		       gncTaxTableGetName (ttw->current_table));
    gnc_error_dialog (ttw->dialog, "%s", message);
    g_free (message);
    return;
  }

  if (gnc_verify_dialog (ttw->dialog, FALSE,
			 _("Are you sure you want to delete \"%s\"?"),
			 gncTaxTableGetName (ttw->current_table))) {
    /* Ok, let's remove it */
    gnc_suspend_gui_refresh ();
    gncTaxTableBeginEdit (ttw->current_table);
    gncTaxTableDestroy (ttw->current_table);
    ttw->current_table = NULL;
    ttw->current_entry = NULL;
    gnc_resume_gui_refresh ();
  }
}

void
tax_table_new_entry_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);
  if (!ttw->current_table)
    return;
  new_tax_table_dialog (ttw, FALSE, NULL, NULL);
}

void
tax_table_edit_entry_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);
  if (!ttw->current_entry)
    return;
  new_tax_table_dialog (ttw, FALSE, ttw->current_entry, NULL);
}

void
tax_table_delete_entry_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);
  if (!ttw->current_table || !ttw->current_entry)
    return;

  if (g_list_length (gncTaxTableGetEntries (ttw->current_table)) <= 1) {
    char *message = _("You cannot remove the last entry from the tax table.\n"
		      "Try deleting the tax table if you want to do that.");
    gnc_error_dialog (ttw->dialog, message);
    return;
  }

  if (gnc_verify_dialog (ttw->dialog, FALSE,
			 _("Are you sure you want to delete this entry?"))) {
    /* Ok, let's remove it */
    gnc_suspend_gui_refresh ();
    gncTaxTableBeginEdit (ttw->current_table);
    gncTaxTableRemoveEntry (ttw->current_table, ttw->current_entry);
    gncTaxTableEntryDestroy (ttw->current_entry);
    gncTaxTableChanged (ttw->current_table);
    gncTaxTableCommitEdit (ttw->current_table);
    ttw->current_entry = NULL;
    gnc_resume_gui_refresh ();
  }
}

static void
tax_table_window_refresh_handler (GHashTable *changes, gpointer data)
{
  TaxTableWindow *ttw = data;

  g_return_if_fail (data);
  tax_table_window_refresh (ttw);
}

static void
tax_table_window_close_handler (gpointer data)
{
  TaxTableWindow *ttw = data;
  g_return_if_fail (ttw);

  gtk_widget_destroy (ttw->dialog);
}

void
tax_table_window_close (GtkWidget *widget, gpointer data)
{
  TaxTableWindow *ttw = data;

  gnc_save_window_size (GCONF_SECTION, GTK_WINDOW (ttw->dialog));
  gnc_ui_tax_table_window_destroy (ttw);
}

void
tax_table_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  TaxTableWindow *ttw = data;

  if (!ttw) return;

  gnc_unregister_gui_component (ttw->component_id);

  g_free (ttw);
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
  TaxTableWindow *ttw = user_data;
  GNCBook *book = find_data;

  return (ttw != NULL && ttw->book == book);
}

/* Create a tax-table window */
TaxTableWindow *
gnc_ui_tax_table_window_new (GNCBook *book)
{
  TaxTableWindow *ttw;
  GladeXML *xml;

  if (!book) return NULL;

  /*
   * Find an existing tax-table window.  If found, bring it to
   * the front.  If we have an actual owner, then set it in
   * the window.
   */
  ttw = gnc_find_first_gui_component (DIALOG_TAX_TABLE_CM_CLASS, find_handler,
				      book);
  if (ttw) {
    gtk_window_present (GTK_WINDOW(ttw->dialog));
    return ttw;
  }

  /* Didn't find one -- create a new window */
  ttw = g_new0 (TaxTableWindow, 1);
  ttw->book = book;

  /* Open and read the XML */
  xml = gnc_glade_xml_new ("tax-tables.glade", "Tax Table Window");
  ttw->dialog = glade_xml_get_widget (xml, "Tax Table Window");
  ttw->names_clist = glade_xml_get_widget (xml, "tax_tables_clist");
  ttw->entries_clist = glade_xml_get_widget (xml, "tax_table_entries");

  /* Setup signals */
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     ttw);

  /* register with component manager */
  ttw->component_id =
    gnc_register_gui_component (DIALOG_TAX_TABLE_CM_CLASS,
				tax_table_window_refresh_handler,
				tax_table_window_close_handler,
				ttw);

  tax_table_window_refresh (ttw);
  gnc_restore_window_size (GCONF_SECTION, GTK_WINDOW (ttw->dialog));
  gtk_widget_show_all (ttw->dialog);
  return ttw;
}

/* Destroy a tax-table window */
void
gnc_ui_tax_table_window_destroy (TaxTableWindow *ttw)
{
  if (!ttw)
    return;

  gnc_close_gui_component (ttw->component_id);
}

/* Create a new tax-table by name */
GncTaxTable *
gnc_ui_tax_table_new_from_name (GNCBook *book, const char *name)
{
  TaxTableWindow *ttw;

  if (!book) return NULL;

  ttw = gnc_ui_tax_table_window_new (book);
  if (!ttw) return NULL;

  return new_tax_table_dialog (ttw, TRUE, NULL, name);
}
