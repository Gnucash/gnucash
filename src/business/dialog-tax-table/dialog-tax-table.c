/*
 * dialog-tax-table.c -- Dialog to create and edit tax-tables
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "gnc-amount-edit.h"
#include "gnc-account-tree.h"
#include "gnc-numeric.h"

#include "gncTaxTable.h"
#include "dialog-tax-table.h"

#define DIALOG_TAX_TABLE_CM_CLASS "tax-table-dialog"

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


static void
new_tax_table_ok_cb (GtkWidget *widget, gpointer data)
{
  NewTaxTable *ntt = data;
  TaxTableWindow *ttw;
  char *name = NULL;
  char *message;
  Account *acc;
  gnc_numeric amount;

  g_return_if_fail (ntt);
  ttw = ntt->ttw;

  /* Verify that we've got real, valid data */

  /* verify the name, maybe */
  if (ntt->new_table) {
    name = gtk_entry_get_text (GTK_ENTRY (ntt->name_entry));
    if (name == NULL || *name == '\0') {
      message = _("You must provide a name for this Tax Table.");
      gnc_error_dialog_parented (GTK_WINDOW (ntt->dialog), message);
      return;
    }
    if (gncTaxTableLookupByName (ttw->book, name)) {
      message = g_strdup_printf(_(
			 "You must provide a unique name for this Tax Table.\n"
			 "Your choice \"%s\" is already in use."), name);
      gnc_error_dialog_parented (GTK_WINDOW (ntt->dialog), message);
      g_free (message);
      return;
    }
  }

  /* verify the amount */
  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (ntt->amount_entry));
  if (gnc_numeric_negative_p (amount)) {
    message = _("Negative amounts are not allowed.");
    gnc_error_dialog_parented (GTK_WINDOW (ntt->dialog), message);
    return;
  }
  if (ntt->type == GNC_AMT_TYPE_PERCENT &&
      gnc_numeric_compare (amount,
			   gnc_numeric_create (100, 1)) > 0) {
    message = _("Percentage amount must be between 0 and 100.");
    gnc_error_dialog_parented (GTK_WINDOW (ntt->dialog), message);
    return;
  }							   

  /* verify the account */
  acc =
    gnc_account_tree_get_current_account (GNC_ACCOUNT_TREE (ntt->acct_tree));
  if (acc == NULL) {
    message = _("You must choose a Tax Account.");
    gnc_error_dialog_parented (GTK_WINDOW (ntt->dialog), message);
    return;
  }

  gnc_suspend_gui_refresh ();

  /* Ok, it's all valid, now either change to add this thing */
  if (ntt->new_table) {
    GncTaxTable *table = gncTaxTableCreate (ttw->book);
    gncTaxTableSetName (table, name);
    /* Reset the current table */
    ttw->current_table = table;
    ntt->created_table = table;
  }

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

  /* Then close/refresh the dialog/window */
  gnome_dialog_close (GNOME_DIALOG (ntt->dialog));
}

static void
new_tax_table_cancel_cb (GtkWidget *widget, gpointer data)
{
  NewTaxTable *ntt = data;
  g_return_if_fail (ntt);
  gnome_dialog_close (GNOME_DIALOG (ntt->dialog));
}

static void
new_tax_table_dialog_destroy_cb (GtkWidget *widget, gpointer data)
{
  NewTaxTable *ntt = data;

  if (!ntt) return;
  g_free (ntt);
}

static void
optionmenu_changed (GtkWidget *widget, NewTaxTable *ntt)
{
  g_return_if_fail (ntt);
  ntt->type = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (widget),
						    "option"));
}

static GtkWidget *
add_menu_item (GtkWidget *menu, NewTaxTable *ntt, char *label, gint type)
	       
{
  GtkWidget *item;

  item = gtk_menu_item_new_with_label (label);
  gtk_object_set_data (GTK_OBJECT (item), "option", GINT_TO_POINTER (type));
  gtk_signal_connect (GTK_OBJECT (item), "activate", optionmenu_changed, ntt);
  gtk_menu_append (GTK_MENU (menu), item);
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
  percent = add_menu_item (menu, ntt, _("Percent %"), GNC_AMT_TYPE_PERCENT);

  gtk_option_menu_set_menu (GTK_OPTION_MENU (omenu), menu);

  gtk_signal_emit_by_name (GTK_OBJECT ((current == GNC_AMT_TYPE_VALUE-1 ?
					value : percent)), "activate", ntt);
  gtk_option_menu_set_history (GTK_OPTION_MENU (omenu), current);
  return menu;
}

static NewTaxTable *
new_tax_table_dialog (TaxTableWindow *ttw, gboolean new_table,
		      GncTaxTableEntry *entry)
{
  NewTaxTable *ntt;
  GladeXML *xml;
  GtkWidget *box, *widget;

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

  /* Create the menu */
  make_menu (glade_xml_get_widget (xml, "type_menu"), ntt);

  /* Attach our own widgets */
  box = glade_xml_get_widget (xml, "amount_box");
  ntt->amount_entry = widget = gnc_amount_edit_new ();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (widget), TRUE);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (widget), 100000);
  gtk_box_pack_start (GTK_BOX (box), widget, TRUE, TRUE, 0);

  box = glade_xml_get_widget (xml, "acct_window");
  ntt->acct_tree = widget = gnc_account_tree_new ();
  gtk_clist_column_titles_hide (GTK_CLIST (widget));
  gnc_account_tree_hide_all_but_name (GNC_ACCOUNT_TREE (widget));
  gtk_container_add (GTK_CONTAINER (box), widget);
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(ntt->acct_tree));

  /* Make 'enter' do the right thing */
  gnome_dialog_set_default (GNOME_DIALOG (ntt->dialog), 0);
  gnome_dialog_editable_enters (GNOME_DIALOG (ntt->dialog),
				GTK_EDITABLE (ntt->name_entry));
  gnome_dialog_editable_enters (GNOME_DIALOG (ntt->dialog),
				GTK_EDITABLE (gnc_amount_edit_gtk_entry
					      (GNC_AMOUNT_EDIT (ntt->amount_entry))));

  /* Connect the dialog buttons */
  gnome_dialog_button_connect (GNOME_DIALOG (ntt->dialog), 0,
			       new_tax_table_ok_cb, ntt);

  gnome_dialog_button_connect (GNOME_DIALOG (ntt->dialog), 1,
			       new_tax_table_cancel_cb, ntt);

  /* Fill in the widgets appropriately */
  if (entry) {
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ntt->amount_entry),
				gncTaxTableEntryGetAmount (entry));
    gnc_account_tree_select_account (GNC_ACCOUNT_TREE (ntt->acct_tree),
				     gncTaxTableEntryGetAccount (entry), TRUE);
  }

  /* Set our modality */
  gnome_dialog_set_parent (GNOME_DIALOG (ntt->dialog),
			   GTK_WINDOW (ttw->dialog));
  gtk_window_set_modal (GTK_WINDOW (ntt->dialog), TRUE);

  gtk_signal_connect (GTK_OBJECT (ntt->dialog), "destroy",
		      new_tax_table_dialog_destroy_cb, ntt);

  /* Show what we should */
  gtk_widget_show_all (ntt->dialog);
  if (new_table == FALSE) {
    widget = glade_xml_get_widget (xml, "table_frame");
    gtk_widget_hide_all (widget);
    gtk_widget_grab_focus (gnc_amount_edit_gtk_entry
			   (GNC_AMOUNT_EDIT (ntt->amount_entry)));
  } else
    gtk_widget_grab_focus (ntt->name_entry);

  return ntt;
}

/***********************************************************************/

static void
tax_table_entries_refresh (TaxTableWindow *ttw, gboolean new_table)
{
  GList *list;
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
  for ( ; list; list = list->next) {
    char *row_text[3];
    gint row;
    GncTaxTableEntry *entry = list->data;
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
  GList *list;
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

  for ( ; list; list = list->next) {
    char *row_text[2];
    gint row;
    GncTaxTable *table = list->data;

    gnc_gui_component_watch_entity (ttw->component_id,
				    gncTaxTableGetGUID (table),
				    GNC_EVENT_MODIFY);

    row_text[0] = (char *)gncTaxTableGetName (table);
    row_text[1] = NULL;

    row = gtk_clist_prepend (clist, row_text);
    gtk_clist_set_row_data (clist, row, table);
    gtk_clist_set_selectable (clist, row, TRUE);
  }

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

static void
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

static void
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
    new_tax_table_dialog (ttw, FALSE, entry);
}

static void
tax_table_new_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);
  new_tax_table_dialog (ttw, TRUE, NULL);
}

static void
tax_table_delete_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);

  if (!ttw->current_table)
    return;

  if (gncTaxTableGetRefcount (ttw->current_table) > 0) {
    char *message =
      g_strdup_printf (_("Tax table \"%s\" is in use.  You cannot delete it."),
		       gncTaxTableGetName (ttw->current_table));
    gnc_error_dialog_parented (GTK_WINDOW (ttw->dialog), message);
    g_free (message);
    return;
  }

  if (gnc_verify_dialog_parented (ttw->dialog, FALSE,
				  _("Are you sure you want to delete \"%s\"?"),
				  gncTaxTableGetName (ttw->current_table))) {
    /* Ok, let's remove it */
    gnc_suspend_gui_refresh ();
    gncTaxTableDestroy (ttw->current_table);
    //    gncTaxTableCommitEdit (ttw->current_table);
    ttw->current_table = NULL;
    gnc_resume_gui_refresh ();
  }
}

static void
tax_table_new_entry_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);
  if (!ttw->current_table)
    return;
  new_tax_table_dialog (ttw, FALSE, NULL);
}

static void
tax_table_edit_entry_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);
  if (!ttw->current_entry)
    return;
  new_tax_table_dialog (ttw, FALSE, ttw->current_entry);
}

static void
tax_table_delete_entry_cb (GtkButton *button, TaxTableWindow *ttw)
{
  g_return_if_fail (ttw);
  if (!ttw->current_table || !ttw->current_entry)
    return;

  if (g_list_length (gncTaxTableGetEntries (ttw->current_table)) <= 1) {
    char *message = _("You cannot remove the last entry from the tax table.\n"
		      "Try deleting the tax table if you want to do that.");
    gnc_error_dialog_parented (GTK_WINDOW (ttw->dialog), message);
    return;
  }

  if (gnc_verify_dialog_parented (ttw->dialog, FALSE,
			  _("Are you sure you want to delete this entry?"))) {
    /* Ok, let's remove it */
    gnc_suspend_gui_refresh ();
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
  gnome_dialog_close (GNOME_DIALOG (ttw->dialog));
}

static void
tax_table_window_close (GtkWidget *widget, gpointer data)
{
  TaxTableWindow *ttw = data;

  gnc_ui_tax_table_window_destroy (ttw);
}

static void
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
  GtkWidget *button;

  if (!book) return NULL;

  /*
   * Find an existing tax-tab;e window.  If found, bring it to
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

  /* Connect all the buttons */
  button = glade_xml_get_widget (xml, "new_table_button");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      tax_table_new_table_cb, ttw);
  button = glade_xml_get_widget (xml, "delete_table_button");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      tax_table_delete_table_cb, ttw);
  button = glade_xml_get_widget (xml, "new_entry_button");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      tax_table_new_entry_cb, ttw);
  button = glade_xml_get_widget (xml, "edit_entry_button");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      tax_table_edit_entry_cb, ttw);
  button = glade_xml_get_widget (xml, "delete_entry_button");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      tax_table_delete_entry_cb, ttw);

  /* Set the row-select callbacks */
  gtk_signal_connect (GTK_OBJECT (ttw->names_clist), "select-row",
		      tax_table_row_selected, ttw);
  gtk_signal_connect (GTK_OBJECT (ttw->entries_clist), "select-row",
		      tax_table_entry_row_selected, ttw);

  /* Connect the dialog buttons */
  gnome_dialog_button_connect (GNOME_DIALOG (ttw->dialog), 0,
			       tax_table_window_close, ttw);

  gtk_signal_connect (GTK_OBJECT (ttw->dialog), "destroy",
		      tax_table_window_destroy_cb, ttw);

  /* register with component manager */
  ttw->component_id =
    gnc_register_gui_component (DIALOG_TAX_TABLE_CM_CLASS,
				tax_table_window_refresh_handler,
				tax_table_window_close_handler,
				ttw);

  tax_table_window_refresh (ttw);
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

static int
from_name_close_cb (GnomeDialog *dialog, gpointer data)
{
  NewTaxTable *ntt;
  GncTaxTable **created_table = data;

  ntt = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");

  *created_table = ntt->created_table;

  gtk_main_quit ();

  return FALSE;
}

/* Create a new tax-table by name */
GncTaxTable *
gnc_ui_tax_table_new_from_name (GNCBook *book, const char *name)
{
  GncTaxTable *created_table = NULL;
  TaxTableWindow *ttw;
  NewTaxTable *ntt;

  if (!book) return NULL;

  ttw = gnc_ui_tax_table_window_new (book);
  if (!ttw) return NULL;

  ntt = new_tax_table_dialog (ttw, TRUE, NULL);
  if (!ntt) return NULL;

  gtk_object_set_data (GTK_OBJECT (ntt->dialog), "dialog_info", ntt);
  gtk_signal_connect (GTK_OBJECT (ntt->dialog), "close",
		      GTK_SIGNAL_FUNC (from_name_close_cb), &created_table);

  /* Preset the name in the new dialog */
  if (name)
    gtk_entry_set_text (GTK_ENTRY (ntt->name_entry), name);

  /* I know that NTT is already modal, no need to reset it here */

  /* Now run the dialog -- wait for it to close */
  gtk_main ();

  return created_table;
}
