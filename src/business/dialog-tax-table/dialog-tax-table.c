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
#include "gnc-gtk-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-amount-edit.h"
#include "gnc-tree-view-account.h"

#include "gncTaxTable.h"
#include "dialog-tax-table.h"

#define DIALOG_TAX_TABLE_CM_CLASS "tax-table-dialog"
#define GCONF_SECTION "dialogs/business/tax_tables"

enum tax_table_cols {
  TAX_TABLE_COL_NAME = 0,
  TAX_TABLE_COL_POINTER,
  NUM_TAX_TABLE_COLS
};

enum tax_entry_cols {
  TAX_ENTRY_COL_NAME = 0,
  TAX_ENTRY_COL_POINTER,
  TAX_ENTRY_COL_AMOUNT,
  NUM_TAX_ENTRY_COLS
};

void tax_table_new_table_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_delete_table_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_new_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_edit_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_delete_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_window_close (GtkWidget *widget, gpointer data);
void tax_table_window_destroy_cb (GtkWidget *widget, gpointer data);


struct _taxtable_window {
  GtkWidget *	dialog;
  GtkWidget *	names_view;
  GtkWidget *	entries_view;

  GncTaxTable *	current_table;
  GncTaxTableEntry *	current_entry;
  QofBook *	book;
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
      gnc_error_dialog (ntt->dialog, "%s", message);
      return FALSE;
    }
    if (gncTaxTableLookupByName (ttw->book, name)) {
      message = g_strdup_printf(_(
			 "You must provide a unique name for this Tax Table. "
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
    gnc_error_dialog (ntt->dialog, "%s", message);
    return FALSE;
  }
  if (ntt->type == GNC_AMT_TYPE_PERCENT &&
      gnc_numeric_compare (amount,
			   gnc_numeric_create (100, 1)) > 0) {
    message = _("Percentage amount must be between 0 and 100.");
    gnc_error_dialog (ntt->dialog, "%s", message);
    return FALSE;
  }							   

  /* verify the account */
  acc = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(ntt->acct_tree));
  if (acc == NULL) {
    message = _("You must choose a Tax Account.");
    gnc_error_dialog (ntt->dialog, "%s", message);
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
combo_changed (GtkWidget *widget, NewTaxTable *ntt)
{
  gint index;

  g_return_if_fail(GTK_IS_COMBO_BOX(widget));
  g_return_if_fail(ntt);

  index = gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
  ntt->type = index + 1;
}

static GncTaxTable *
new_tax_table_dialog (TaxTableWindow *ttw, gboolean new_table,
		      GncTaxTableEntry *entry, const char *name)
{
  GncTaxTable *created_table = NULL;
  NewTaxTable *ntt;
  GladeXML *xml;
  GtkWidget *box, *widget, *combo;
  gboolean done;
  gint response, index;

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
  combo = glade_xml_get_widget (xml, "type_combobox");
  index = ntt->type ? ntt->type : GNC_AMT_TYPE_VALUE;
  gtk_combo_box_set_active(GTK_COMBO_BOX(combo), index - 1);
  g_signal_connect (combo, "changed", G_CALLBACK (combo_changed), ntt);

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
tax_table_entries_refresh (TaxTableWindow *ttw)
{
  GList *list, *node;
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeIter iter;
  GtkTreePath *path;
  GtkTreeSelection *selection;
  GtkTreeRowReference *reference = NULL;
  GncTaxTableEntry *selected_entry;

  g_return_if_fail (ttw);

  view = GTK_TREE_VIEW (ttw->entries_view);
  store = GTK_LIST_STORE(gtk_tree_view_get_model(view));

  /* Clear the list */
  selected_entry = ttw->current_entry;
  gtk_list_store_clear (store);
  if (ttw->current_table == NULL)
    return;

  /* Add the items to the list */
  list = gncTaxTableGetEntries (ttw->current_table);
  if (list)
    list = g_list_reverse (g_list_copy (list));

  for (node = list ; node; node = node->next) {
    char *row_text[3];
    GncTaxTableEntry *entry = node->data;
    Account *acc = gncTaxTableEntryGetAccount (entry);
    gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

    row_text[0] = gnc_account_get_full_name (acc);
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

    gtk_list_store_prepend(store, &iter);
    gtk_list_store_set(store, &iter,
		       TAX_ENTRY_COL_NAME, row_text[0],
		       TAX_ENTRY_COL_POINTER, entry,
		       TAX_ENTRY_COL_AMOUNT, row_text[1],
		       -1);
    if (entry == selected_entry) {
      path = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter);
      reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(store), path);
      gtk_tree_path_free(path);
    }

    g_free (row_text[0]);
    g_free (row_text[1]);
  }

  if (reference) {
    path = gtk_tree_row_reference_get_path(reference);
    gtk_tree_row_reference_free(reference);
    if (path) {
      selection = gtk_tree_view_get_selection(view);
      gtk_tree_selection_select_path(selection, path);
      gtk_tree_view_scroll_to_cell(view, path, NULL, TRUE, 0.5, 0.0);
      gtk_tree_path_free(path);
    }
  }
}

static void
tax_table_window_refresh (TaxTableWindow *ttw)
{
  GList *list, *node;
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeIter iter;
  GtkTreePath *path;
  GtkTreeSelection *selection;
  GtkTreeRowReference *reference = NULL;

  g_return_if_fail (ttw);
  view = GTK_TREE_VIEW (ttw->names_view);
  store = GTK_LIST_STORE(gtk_tree_view_get_model(view));

  /* Clear the list */
  gtk_list_store_clear(store);

  gnc_gui_component_clear_watches (ttw->component_id);

  /* Add the items to the list */
  list = gncTaxTableGetTables (ttw->book);
  if (list)    
    list = g_list_reverse (g_list_copy (list));

  for (node = list; node; node = node->next) {
    GncTaxTable *table = node->data;

    gnc_gui_component_watch_entity (ttw->component_id,
				    gncTaxTableGetGUID (table),
				    QOF_EVENT_MODIFY);

    gtk_list_store_prepend(store, &iter);
    gtk_list_store_set(store, &iter,
		       TAX_TABLE_COL_NAME, gncTaxTableGetName (table),
		       TAX_TABLE_COL_POINTER, table,
		       -1);
    if (table == ttw->current_table) {
      path = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter);
      reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(store), path);
      gtk_tree_path_free(path);
    }
  }

  if (list)
    g_list_free (list);

  gnc_gui_component_watch_entity_type (ttw->component_id,
				       GNC_TAXTABLE_MODULE_NAME,
				       QOF_EVENT_CREATE | QOF_EVENT_DESTROY);

  if (reference) {
    path = gtk_tree_row_reference_get_path(reference);
    gtk_tree_row_reference_free(reference);
    if (path) {
      selection = gtk_tree_view_get_selection(view);
      gtk_tree_selection_select_path(selection, path);
      gtk_tree_view_scroll_to_cell(view, path, NULL, TRUE, 0.5, 0.0);
      gtk_tree_path_free(path);
    }
  }
  ;;  tax_table_entries_refresh (ttw);
  /* select_row() above will refresh the entries window */
}

static void
tax_table_selection_changed (GtkTreeSelection *selection,
			     gpointer          user_data)
{
  TaxTableWindow *ttw = user_data;
  GncTaxTable *table;
  GtkTreeModel *model;
  GtkTreeIter iter;

  g_return_if_fail (ttw);

  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return;

  gtk_tree_model_get(model, &iter, TAX_TABLE_COL_POINTER, &table, -1);
  g_return_if_fail (table);

  /* If we've changed, then reset the entry list */
  if (table != ttw->current_table) {
    ttw->current_table = table;
    ttw->current_entry = NULL;
  }
  /* And force a refresh of the entries */
  tax_table_entries_refresh (ttw);
}

static void
tax_table_entry_selection_changed (GtkTreeSelection *selection,
				   gpointer          user_data)
{
  TaxTableWindow *ttw = user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;

  g_return_if_fail (ttw);

  if (!gtk_tree_selection_get_selected(selection, &model, &iter)) {
    ttw->current_entry = NULL;
    return;
  }

  gtk_tree_model_get(model, &iter, TAX_ENTRY_COL_POINTER, &ttw->current_entry, -1);
}

static void
tax_table_entry_row_activated (GtkTreeView       *tree_view,
			       GtkTreePath       *path,
			       GtkTreeViewColumn *column,
			       gpointer           user_data)
{
  TaxTableWindow *ttw = user_data;

  new_tax_table_dialog (ttw, FALSE, ttw->current_entry, NULL);
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
    char *message = _("You cannot remove the last entry from the tax table. "
		      "Try deleting the tax table if you want to do that.");
    gnc_error_dialog (ttw->dialog, "%s", message);
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
  QofBook *book = find_data;

  return (ttw != NULL && ttw->book == book);
}

/* Create a tax-table window */
TaxTableWindow *
gnc_ui_tax_table_window_new (QofBook *book)
{
  TaxTableWindow *ttw;
  GladeXML *xml;
  GtkTreeView *view;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GtkListStore *store;
  GtkTreeSelection *selection;

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
  ttw->names_view = glade_xml_get_widget (xml, "tax_tables_view");
  ttw->entries_view = glade_xml_get_widget (xml, "tax_table_entries");

  /* Create the tax tables view */
  view = GTK_TREE_VIEW(ttw->names_view);
  store = gtk_list_store_new (NUM_TAX_TABLE_COLS, G_TYPE_STRING,
			      G_TYPE_POINTER);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("", renderer,
						    "text", TAX_TABLE_COL_NAME,
						    NULL);
  gtk_tree_view_append_column(view, column);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(tax_table_selection_changed), ttw);


  /* Create the tax table entries view */
  view = GTK_TREE_VIEW(ttw->entries_view);
  store = gtk_list_store_new (NUM_TAX_ENTRY_COLS, G_TYPE_STRING,
			      G_TYPE_POINTER, G_TYPE_STRING);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("", renderer,
						    "text", TAX_ENTRY_COL_NAME,
						    NULL);
  gtk_tree_view_append_column(view, column);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(tax_table_entry_selection_changed), ttw);
  g_signal_connect(view, "row-activated",
		   G_CALLBACK(tax_table_entry_row_activated), ttw);


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
gnc_ui_tax_table_new_from_name (QofBook *book, const char *name)
{
  TaxTableWindow *ttw;

  if (!book) return NULL;

  ttw = gnc_ui_tax_table_window_new (book);
  if (!ttw) return NULL;

  return new_tax_table_dialog (ttw, TRUE, NULL, name);
}
