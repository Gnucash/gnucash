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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-gtk-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-amount-edit.h"
#include "gnc-tree-view-account.h"
#include "gnc-account-sel.h"

#include "gncTaxTable.h"
#include "dialog-tax-table.h"

#define DIALOG_TAX_TABLE_CM_CLASS "tax-table-dialog"
#define GNC_PREFS_GROUP "dialogs.business.tax-tables"

void tax_table_new_table_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_rename_table_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_delete_table_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_new_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_edit_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_delete_entry_cb (GtkButton *button, TaxTableWindow *ttw);
void tax_table_window_close (GtkWidget *widget, gpointer data);
void tax_table_window_destroy_cb (GtkWidget *widget, gpointer data);

struct _taxtable_window
{
    GtkWidget *dialog;
    GtkWidget *names_view;
    GtkWidget *entries_view;
    GListStore *tables_model;
    GtkSingleSelection *tables_selection;
    GListStore *entries_model;
    GtkSingleSelection *entries_selection;

    GncTaxTable      *current_table;
    GncTaxTableEntry *current_entry;
    QofBook          *book;
    gint              component_id;
    QofSession       *session;
};

typedef struct _new_taxtable
{
    GtkWidget *dialog;
    GtkWidget *name_entry;
    GtkWidget *amount_entry;
    GtkWidget *acct_tree;
    GtkDropDown *type_dropdown;

    GncTaxTable      *created_table;
    TaxTableWindow   *ttw;
    GncTaxTableEntry *entry;
    gint              type;
    gboolean          new_table;
    gboolean          completed;
} NewTaxTable;
typedef struct
{
    TaxTableWindow *ttw;
    GWeakRef parent;
    GtkWindow *window;
    GtkEntry *entry;
    GncGUID table_guid;
} TaxTableRenameRequest;

static GObject *
tax_table_row_new (const char *name, GncTaxTable *table)
{
    GObject *row = g_object_new (G_TYPE_OBJECT, NULL);
    g_object_set_data_full (row, "tax-table-name", g_strdup (name), g_free);
    g_object_set_data (row, "tax-table", table);
    return row;
}

static GObject *
tax_table_entry_row_new (const char *name, const char *amount,
                         GncTaxTableEntry *entry)
{
    GObject *row = g_object_new (G_TYPE_OBJECT, NULL);
    g_object_set_data_full (row, "tax-entry-name", g_strdup (name), g_free);
    g_object_set_data_full (row, "tax-entry-amount", g_strdup (amount), g_free);
    g_object_set_data (row, "tax-entry", entry);
    return row;
}

static void
tax_table_factory_setup (GtkSignalListItemFactory *factory, GtkListItem *item,
                         gpointer user_data)
{
    (void)factory;
    (void)user_data;
    gtk_list_item_set_child (item, gtk_label_new (NULL));
}

static void
tax_table_factory_bind (GtkSignalListItemFactory *factory, GtkListItem *item,
                        gpointer user_data)
{
    GObject *row = gtk_list_item_get_item (item);
    const char *key = user_data;
    (void)factory;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        g_object_get_data (row, key));
}

static GtkColumnViewColumn *
tax_table_column_new (const char *title, const char *key)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *column;
    g_signal_connect (factory, "setup", G_CALLBACK (tax_table_factory_setup), NULL);
    g_signal_connect_data (factory, "bind", G_CALLBACK (tax_table_factory_bind),
                           (gpointer)key, NULL, 0);
    column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_expand (column, TRUE);
    return column;
}

static gboolean
new_tax_table_check_entry (NewTaxTable *ntt, GError **error)
{
    GNCPrintAmountInfo print_info;
    gnc_numeric value;
    gint result;
    GError *tmp_error = NULL;

    if (ntt->type == GNC_AMT_TYPE_VALUE)
    {
        Account *acc = gnc_tree_view_account_get_selected_account
            (GNC_TREE_VIEW_ACCOUNT (ntt->acct_tree));
        if (acc)
        {
            gnc_commodity *currency = xaccAccountGetCommodity (acc);
            print_info = gnc_commodity_print_info (currency, FALSE);
            gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (ntt->amount_entry),
                                          gnc_commodity_get_fraction (currency));
        }
        else
        {
            print_info = gnc_integral_print_info ();
            gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (ntt->amount_entry), 100000);
        }
    }
    else
    {
        print_info = gnc_integral_print_info ();
        print_info.max_decimal_places = 5;
        gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (ntt->amount_entry), 100000);
    }

    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT(ntt->amount_entry), print_info);

    result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(ntt->amount_entry),
                                            &value, TRUE, &tmp_error);

    if (result == 1)
    {
        if (error)
            g_propagate_error (error, tmp_error);
        else
            g_error_free (tmp_error);
        return FALSE;
    }
    return TRUE;
}

static gboolean
new_tax_table_ok_cb (NewTaxTable *ntt)
{
    TaxTableWindow *ttw;
    const char *name = NULL;
    char *message;
    Account *acc;
    gnc_numeric amount;
    GError *error = NULL;

    g_return_val_if_fail (ntt, FALSE);
    ttw = ntt->ttw;

    /* Verify that we've got real, valid data */

    /* verify the name, maybe */
    if (ntt->new_table)
    {
        name = gnc_entry_get_text (GTK_ENTRY(ntt->name_entry));
        if (name == NULL || *name == '\0')
        {
            message = _("You must provide a name for this Tax Table.");
            gnc_error_dialog (GTK_WINDOW(ntt->dialog), "%s", message);
            return FALSE;
        }
        if (gncTaxTableLookupByName (ttw->book, name))
        {
            message = g_strdup_printf (_(
                                          "You must provide a unique name for this Tax Table. "
                                          "Your choice \"%s\" is already in use."), name);
            gnc_error_dialog (GTK_WINDOW(ntt->dialog), "%s", message);
            g_free (message);
            return FALSE;
        }
    }

    /* test for valid value */
    if (!new_tax_table_check_entry (ntt, &error))
    {
        message = g_strdup (error->message);
        gnc_error_dialog (GTK_WINDOW(ntt->dialog), "%s", message);
        g_free (message);
        g_error_free (error);
        return FALSE;
    }

    /* verify the amount. Note that negative values are allowed (required for European tax rules) */
    amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT(ntt->amount_entry));
    if (ntt->type == GNC_AMT_TYPE_PERCENT &&
            gnc_numeric_compare (gnc_numeric_abs (amount),
                                 gnc_numeric_create (100, 1)) > 0)
    {
        message = _("Percentage amount must be between -100 and 100.");
        gnc_error_dialog (GTK_WINDOW(ntt->dialog), "%s", message);
        return FALSE;
    }

    /* verify the account */
    acc = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(ntt->acct_tree));
    if (acc == NULL)
    {
        message = _("You must choose a Tax Account.");
        gnc_error_dialog (GTK_WINDOW(ntt->dialog), "%s", message);
        return FALSE;
    }

    gnc_suspend_gui_refresh ();

    /* Ok, it's all valid, now either change to add this thing */
    if (ntt->new_table)
    {
        GncTaxTable *table = gncTaxTableCreate (ttw->book);
        gncTaxTableBeginEdit (table);
        gncTaxTableSetName (table, name);
        /* Reset the current table */
        ttw->current_table = table;
        ntt->created_table = table;
    }
    else
        gncTaxTableBeginEdit (ttw->current_table);

    /* Create/edit the entry */
    {
        GncTaxTableEntry *entry;

        if (ntt->entry)
        {
            entry = ntt->entry;
        }
        else
        {
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

    gnc_resume_gui_refresh ();
    return TRUE;
}

static void
combo_changed (GObject *object, GParamSpec *pspec, NewTaxTable *ntt)
{
    (void)object;
    (void)pspec;
    ntt->type = gtk_drop_down_get_selected (ntt->type_dropdown) + 1;
    new_tax_table_check_entry (ntt, NULL);
}

static void
tax_table_account_selection_changed_cb (GtkSelectionModel *selection, guint position,
                                      guint n_items, NewTaxTable *ntt)
{
    new_tax_table_check_entry (ntt, NULL);
    (void)selection;
    (void)position;
    (void)n_items;
}

static void
new_tax_table_finished (NewTaxTable *ntt)
{
    if (!ntt || ntt->completed)
        return;
    ntt->completed = TRUE;
    gtk_window_destroy (GTK_WINDOW (ntt->dialog));
}

static gboolean
new_tax_table_close_request (GtkWindow *window, NewTaxTable *ntt)
{
    (void)window;
    new_tax_table_finished (ntt);
    return TRUE;
}

static void
new_tax_table_destroyed (GtkWidget *widget, NewTaxTable *ntt)
{
    (void)widget;
    ntt->dialog = NULL;
    ntt->completed = TRUE;
    g_free (ntt);
}

static void
new_tax_table_cancel_clicked (GtkButton *button, NewTaxTable *ntt)
{
    (void)button;
    new_tax_table_finished (ntt);
}

static void
new_tax_table_accept_clicked (GtkButton *button, NewTaxTable *ntt)
{
    (void)button;
    if (new_tax_table_ok_cb (ntt))
        new_tax_table_finished (ntt);
}

static void
new_tax_table_dialog (TaxTableWindow *ttw, gboolean new_table,
                      GncTaxTableEntry *entry, const char *name)
{
    NewTaxTable *ntt;
    GtkBuilder *builder;
    GtkWidget *box;
    GtkWidget *widget;
    GtkStringList *types;
    GtkSelectionModel *account_selection;

    if (!ttw || (new_table && entry))
        return;

    ntt = g_new0 (NewTaxTable, 1);
    ntt->ttw = ttw;
    ntt->entry = entry;
    ntt->new_table = new_table;
    ntt->type = entry ? gncTaxTableEntryGetType (entry) : GNC_AMT_TYPE_PERCENT;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-tax-table.glade", "new_tax_table_dialog");
    ntt->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "new_tax_table_dialog"));
    gtk_widget_set_name (ntt->dialog, "gnc-id-tax-table");
    gnc_widget_style_context_add_class (ntt->dialog, "gnc-class-taxes");
    ntt->name_entry = GTK_WIDGET (gtk_builder_get_object (builder, "name_entry"));
    if (name)
        gnc_entry_set_text (GTK_ENTRY (ntt->name_entry), name);

    ntt->type_dropdown = GTK_DROP_DOWN (gtk_builder_get_object (builder, "type_dropdown"));
    types = gtk_string_list_new ((const char * const[]){ _("Value"), _("Percent"), NULL });
    gtk_drop_down_set_model (ntt->type_dropdown, G_LIST_MODEL (types));
    gtk_drop_down_set_selected (ntt->type_dropdown, ntt->type - 1);
    g_object_unref (types);
    g_signal_connect (ntt->type_dropdown, "notify::selected", G_CALLBACK (combo_changed), ntt);

    box = GTK_WIDGET (gtk_builder_get_object (builder, "amount_box"));
    ntt->amount_entry = widget = gnc_amount_edit_new ();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (widget), TRUE);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (widget), 100000);
    gtk_box_append (GTK_BOX (box), widget);

    box = GTK_WIDGET (gtk_builder_get_object (builder, "acct_window"));
    ntt->acct_tree = GTK_WIDGET (gnc_tree_view_account_new (FALSE));
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (box), ntt->acct_tree);
    gnc_tree_view_account_set_headers_visible (GNC_TREE_VIEW_ACCOUNT (ntt->acct_tree), FALSE);
    account_selection = gnc_tree_view_account_get_selection_model (GNC_TREE_VIEW_ACCOUNT (ntt->acct_tree));
    g_signal_connect (account_selection, "selection-changed",
                      G_CALLBACK (tax_table_account_selection_changed_cb), ntt);

    gtk_entry_set_activates_default (GTK_ENTRY (gnc_amount_edit_gtk_entry
                                    (GNC_AMOUNT_EDIT (ntt->amount_entry))), TRUE);
    widget = GTK_WIDGET (gtk_builder_get_object (builder, "value_label"));
    gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT (ntt->amount_entry), widget);
    widget = GTK_WIDGET (gtk_builder_get_object (builder, "account_label"));
    gtk_label_set_mnemonic_widget (GTK_LABEL (widget), ntt->acct_tree);
    if (entry)
    {
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ntt->amount_entry),
                                    gncTaxTableEntryGetAmount (entry));
        gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT (ntt->acct_tree),
                                                     gncTaxTableEntryGetAccount (entry));
    }

    gtk_window_set_transient_for (GTK_WINDOW (ntt->dialog), GTK_WINDOW (ttw->dialog));
    g_signal_connect_object (ttw->dialog, "destroy", G_CALLBACK (gtk_window_destroy),
                             ntt->dialog, G_CONNECT_SWAPPED);
    g_signal_connect (ntt->dialog, "close-request", G_CALLBACK (new_tax_table_close_request), ntt);
    g_signal_connect (ntt->dialog, "destroy", G_CALLBACK (new_tax_table_destroyed), ntt);
    g_signal_connect (gtk_builder_get_object (builder, "cancel_button"), "clicked",
                      G_CALLBACK (new_tax_table_cancel_clicked), ntt);
    g_signal_connect (gtk_builder_get_object (builder, "ok_button"), "clicked",
                      G_CALLBACK (new_tax_table_accept_clicked), ntt);
    gtk_window_set_default_widget (GTK_WINDOW (ntt->dialog),
                                   GTK_WIDGET (gtk_builder_get_object (builder, "ok_button")));
    if (!new_table)
    {
        gtk_widget_set_visible (GTK_WIDGET (gtk_builder_get_object (builder, "table_title")), FALSE);
        gtk_widget_set_visible (GTK_WIDGET (gtk_builder_get_object (builder, "table_name")), FALSE);
        gtk_widget_set_visible (GTK_WIDGET (gtk_builder_get_object (builder, "spacer")), FALSE);
        gtk_widget_set_visible (ntt->name_entry, FALSE);
        gtk_widget_grab_focus (gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (ntt->amount_entry)));
    }
    else
        gtk_widget_grab_focus (ntt->name_entry);

    g_object_unref (builder);
    gtk_window_present (GTK_WINDOW (ntt->dialog));
}

void
tax_table_new_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
    g_return_if_fail (ttw);
    new_tax_table_dialog (ttw, TRUE, NULL, NULL);
    (void)button;
}

static gint
tax_table_compare (gconstpointer left, gconstpointer right)
{
    return g_utf8_collate (gncTaxTableGetName ((GncTaxTable *)left),
                           gncTaxTableGetName ((GncTaxTable *)right));
}

static gint
tax_table_entry_compare (gconstpointer left, gconstpointer right)
{
    Account *left_account = gncTaxTableEntryGetAccount ((GncTaxTableEntry *)left);
    Account *right_account = gncTaxTableEntryGetAccount ((GncTaxTableEntry *)right);
    char *left_name = gnc_account_get_full_name (left_account);
    char *right_name = gnc_account_get_full_name (right_account);
    gint result = g_utf8_collate (left_name, right_name);
    g_free (left_name);
    g_free (right_name);
    return result;
}

static void
tax_table_select_position (GtkSingleSelection *selection, GtkColumnView *view,
                           guint position)
{
    if (position == GTK_INVALID_LIST_POSITION)
    {
        gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (selection));
        return;
    }
    gtk_selection_model_select_item (GTK_SELECTION_MODEL (selection), position, TRUE);
    gtk_column_view_scroll_to (view, position, NULL, GTK_LIST_SCROLL_FOCUS, NULL);
}

static void
tax_table_entries_refresh (TaxTableWindow *ttw)
{
    GList *entries;
    GList *node;
    guint selected = GTK_INVALID_LIST_POSITION;
    guint position = 0;

    g_return_if_fail (ttw);
    g_list_store_remove_all (ttw->entries_model);
    if (!ttw->current_table)
        return;
    entries = g_list_sort (g_list_copy (gncTaxTableGetEntries (ttw->current_table)),
                           tax_table_entry_compare);
    for (node = entries; node; node = node->next, position++)
    {
        GncTaxTableEntry *entry = node->data;
        Account *account = gncTaxTableEntryGetAccount (entry);
        gnc_numeric amount = gncTaxTableEntryGetAmount (entry);
        char *account_name = gnc_account_get_full_name (account);
        char *amount_name;
        GObject *row;
        if (gncTaxTableEntryGetType (entry) == GNC_AMT_TYPE_PERCENT)
            amount_name = g_strdup_printf ("%s%%", xaccPrintAmount
                (amount, gnc_default_print_info (FALSE)));
        else
            amount_name = g_strdup (xaccPrintAmount (amount, gnc_default_print_info (TRUE)));
        row = tax_table_entry_row_new (account_name, amount_name, entry);
        g_list_store_append (ttw->entries_model, row);
        g_object_unref (row);
        if (entry == ttw->current_entry)
            selected = position;
        g_free (account_name);
        g_free (amount_name);
    }
    g_list_free (entries);
    tax_table_select_position (ttw->entries_selection,
                               GTK_COLUMN_VIEW (ttw->entries_view), selected);
}

static void
tax_table_window_refresh (TaxTableWindow *ttw)
{
    GList *tables;
    GList *node;
    guint selected = GTK_INVALID_LIST_POSITION;
    guint position = 0;

    g_return_if_fail (ttw);
    g_list_store_remove_all (ttw->tables_model);
    gnc_gui_component_clear_watches (ttw->component_id);
    tables = g_list_sort (g_list_copy (gncTaxTableGetTables (ttw->book)), tax_table_compare);
    for (node = tables; node; node = node->next, position++)
    {
        GncTaxTable *table = node->data;
        GObject *row = tax_table_row_new (gncTaxTableGetName (table), table);
        gnc_gui_component_watch_entity (ttw->component_id, gncTaxTableGetGUID (table),
                                        QOF_EVENT_MODIFY);
        g_list_store_append (ttw->tables_model, row);
        g_object_unref (row);
        if (table == ttw->current_table)
            selected = position;
    }
    g_list_free (tables);
    gnc_gui_component_watch_entity_type (ttw->component_id, GNC_TAXTABLE_MODULE_NAME,
                                         QOF_EVENT_CREATE | QOF_EVENT_DESTROY);
    tax_table_select_position (ttw->tables_selection,
                               GTK_COLUMN_VIEW (ttw->names_view), selected);
    tax_table_entries_refresh (ttw);
}

static void
tax_table_selection_changed (GtkSelectionModel *selection, guint position,
                             guint n_items, gpointer user_data)
{
    TaxTableWindow *ttw = user_data;
    guint selected = gtk_single_selection_get_selected (GTK_SINGLE_SELECTION (selection));
    GObject *row;
    GncTaxTable *table;
    (void)position;
    (void)n_items;
    if (selected == GTK_INVALID_LIST_POSITION)
        return;
    row = g_list_model_get_item (G_LIST_MODEL (ttw->tables_selection), selected);
    table = g_object_get_data (row, "tax-table");
    g_object_unref (row);
    if (!table)
        return;
    if (table != ttw->current_table)
    {
        ttw->current_table = table;
        ttw->current_entry = NULL;
    }
    tax_table_entries_refresh (ttw);
}

static void
tax_table_entry_selection_changed (GtkSelectionModel *selection, guint position,
                                   guint n_items, gpointer user_data)
{
    TaxTableWindow *ttw = user_data;
    guint selected = gtk_single_selection_get_selected (GTK_SINGLE_SELECTION (selection));
    GObject *row;
    (void)position;
    (void)n_items;
    if (selected == GTK_INVALID_LIST_POSITION)
    {
        ttw->current_entry = NULL;
        return;
    }
    row = g_list_model_get_item (G_LIST_MODEL (ttw->entries_selection), selected);
    ttw->current_entry = g_object_get_data (row, "tax-entry");
    g_object_unref (row);
}

static void
tax_table_entry_activated (GtkColumnView *view, guint position, gpointer user_data)
{
    TaxTableWindow *ttw = user_data;
    (void)view;
    gtk_selection_model_select_item (GTK_SELECTION_MODEL (ttw->entries_selection), position, TRUE);
    if (ttw->current_entry)
        new_tax_table_dialog (ttw, FALSE, ttw->current_entry, NULL);
}
static void
tax_table_rename_request_free (TaxTableRenameRequest *request)
{
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
tax_table_rename_destroyed (GtkWidget *widget, TaxTableRenameRequest *request)
{
    (void)widget;
    tax_table_rename_request_free (request);
}

static gboolean
tax_table_rename_close_request (GtkWindow *window, TaxTableRenameRequest *request)
{
    (void)request;
    gtk_window_destroy (window);
    return TRUE;
}

static void
tax_table_rename_accept_clicked (GtkButton *button, TaxTableRenameRequest *request)
{
    GtkWidget *parent;
    TaxTableWindow *ttw;
    GncTaxTable *table;
    const char *newname;
    const char *oldname;

    (void)button;
    parent = g_weak_ref_get (&request->parent);
    if (!parent)
    {
        gtk_window_destroy (request->window);
        return;
    }
    ttw = request->ttw;
    table = qof_book_shutting_down (ttw->book) ? NULL :
        gncTaxTableLookup (ttw->book, &request->table_guid);
    newname = gtk_editable_get_text (GTK_EDITABLE (request->entry));
    oldname = table ? gncTaxTableGetName (table) : NULL;
    if (table && newname && *newname && g_strcmp0 (oldname, newname) != 0)
    {
        if (gncTaxTableLookupByName (ttw->book, newname))
        {
            char *message = g_strdup_printf (_("Tax table name \"%s\" already exists."),
                                             newname);
            gnc_error_dialog (GTK_WINDOW (parent), "%s", message);
            g_free (message);
            g_object_unref (parent);
            return;
        }
        gncTaxTableSetName (table, newname);
    }
    g_object_unref (parent);
    gtk_window_destroy (request->window);
}

static void
tax_table_rename_cancel_clicked (GtkButton *button, TaxTableRenameRequest *request)
{
    (void)button;
    gtk_window_destroy (request->window);
}

void
tax_table_rename_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
    TaxTableRenameRequest *request;
    GtkWidget *content;
    GtkWidget *label;
    GtkWidget *buttons;
    GtkWidget *cancel;
    GtkWidget *accept;

    (void)button;
    g_return_if_fail (ttw);
    if (!ttw->current_table)
        return;

    request = g_new0 (TaxTableRenameRequest, 1);
    request->ttw = ttw;
    request->table_guid = gncTaxTableRetGUID (ttw->current_table);
    g_weak_ref_init (&request->parent, ttw->dialog);
    request->window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (request->window);
    gtk_window_set_title (request->window, _("Rename"));
    gtk_window_set_modal (request->window, TRUE);
    gtk_window_set_transient_for (request->window, GTK_WINDOW (ttw->dialog));
    gtk_window_set_resizable (request->window, FALSE);
    g_signal_connect_object (ttw->dialog, "destroy", G_CALLBACK (gtk_window_destroy),
                             request->window, G_CONNECT_SWAPPED);

    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    gtk_widget_set_margin_start (content, 18);
    gtk_widget_set_margin_end (content, 18);
    gtk_widget_set_margin_top (content, 18);
    gtk_widget_set_margin_bottom (content, 18);
    gtk_window_set_child (request->window, content);
    label = gtk_label_new (_("Please enter new name"));
    gtk_widget_set_halign (label, GTK_ALIGN_START);
    gtk_box_append (GTK_BOX (content), label);
    request->entry = GTK_ENTRY (gtk_entry_new ());
    gtk_editable_set_text (GTK_EDITABLE (request->entry), gncTaxTableGetName (ttw->current_table));
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (request->entry));
    buttons = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_halign (buttons, GTK_ALIGN_END);
    cancel = gtk_button_new_with_mnemonic (_("_Cancel"));
    accept = gtk_button_new_with_mnemonic (_("_Rename"));
    gtk_box_append (GTK_BOX (buttons), cancel);
    gtk_box_append (GTK_BOX (buttons), accept);
    gtk_box_append (GTK_BOX (content), buttons);
    gtk_window_set_default_widget (request->window, accept);
    g_signal_connect (accept, "clicked", G_CALLBACK (tax_table_rename_accept_clicked), request);
    g_signal_connect (cancel, "clicked", G_CALLBACK (tax_table_rename_cancel_clicked), request);
    g_signal_connect (request->window, "close-request", G_CALLBACK (tax_table_rename_close_request), request);
    g_signal_connect (request->window, "destroy", G_CALLBACK (tax_table_rename_destroyed), request);
    gtk_widget_grab_focus (GTK_WIDGET (request->entry));
    gtk_window_present (request->window);
}



typedef enum
{
    TAX_TABLE_DELETE,
    TAX_TABLE_ENTRY_DELETE
} TaxTableDeleteKind;

typedef struct
{
    TaxTableWindow *ttw;
    GWeakRef dialog;
    gulong destroy_handler;
    GncGUID table_guid;
    GncTaxTableEntry *entry;
    TaxTableDeleteKind kind;
} TaxTableDeleteRequest;

static void
tax_table_delete_request_destroyed (GtkWidget *dialog,
                                    TaxTableDeleteRequest *request)
{
    (void)dialog;
    request->ttw = NULL;
    request->destroy_handler = 0;
}

static void
tax_table_delete_request_free (TaxTableDeleteRequest *request)
{
    GtkWidget *dialog = g_weak_ref_get (&request->dialog);

    if (dialog && request->destroy_handler)
        g_signal_handler_disconnect (dialog, request->destroy_handler);
    g_clear_object (&dialog);
    g_weak_ref_clear (&request->dialog);
    g_free (request);
}

static void
tax_table_delete_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    TaxTableDeleteRequest *request = user_data;
    GncTaxTable *table = NULL;

    (void)parent;
    if (response == GTK_RESPONSE_YES && request->ttw &&
        !qof_book_shutting_down (request->ttw->book))
        table = gncTaxTableLookup (request->ttw->book, &request->table_guid);

    if (table && request->kind == TAX_TABLE_DELETE &&
        gncTaxTableGetRefcount (table) == 0)
    {
        gnc_suspend_gui_refresh ();
        gncTaxTableBeginEdit (table);
        gncTaxTableDestroy (table);
        if (request->ttw->current_table == table)
        {
            request->ttw->current_table = NULL;
            request->ttw->current_entry = NULL;
        }
        gnc_resume_gui_refresh ();
    }
    else if (table && request->kind == TAX_TABLE_ENTRY_DELETE && request->entry &&
             g_list_length (gncTaxTableGetEntries (table)) > 1 &&
             g_list_find (gncTaxTableGetEntries (table), request->entry))
    {
        gnc_suspend_gui_refresh ();
        gncTaxTableBeginEdit (table);
        gncTaxTableRemoveEntry (table, request->entry);
        gncTaxTableEntryDestroy (request->entry);
        gncTaxTableChanged (table);
        gncTaxTableCommitEdit (table);
        if (request->ttw->current_entry == request->entry)
            request->ttw->current_entry = NULL;
        gnc_resume_gui_refresh ();
    }

    tax_table_delete_request_free (request);
}

static void
tax_table_delete_request (TaxTableWindow *ttw, TaxTableDeleteKind kind)
{
    TaxTableDeleteRequest *request;

    request = g_new0 (TaxTableDeleteRequest, 1);
    request->ttw = ttw;
    g_weak_ref_init (&request->dialog, ttw->dialog);
    request->destroy_handler = g_signal_connect (ttw->dialog, "destroy",
                                                 G_CALLBACK (tax_table_delete_request_destroyed),
                                                 request);
    request->table_guid = gncTaxTableRetGUID (ttw->current_table);
    request->entry = kind == TAX_TABLE_ENTRY_DELETE ? ttw->current_entry : NULL;
    request->kind = kind;

    if (kind == TAX_TABLE_DELETE)
    {
        gnc_verify_dialog_async (GTK_WINDOW (ttw->dialog), FALSE,
                                 tax_table_delete_finished, request,
                                 _("Are you sure you want to delete \"%s\"?"),
                                 gncTaxTableGetName (ttw->current_table));
    }
    else
    {
        gnc_verify_dialog_async (GTK_WINDOW (ttw->dialog), FALSE,
                                 tax_table_delete_finished, request, "%s",
                                 _("Are you sure you want to delete this entry?"));
    }
}

void
tax_table_delete_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
    g_return_if_fail (ttw);

    if (!ttw->current_table)
        return;

    if (gncTaxTableGetRefcount (ttw->current_table) > 0)
    {
        char *message =
            g_strdup_printf (_("Tax table \"%s\" is in use. You cannot delete it."),
                             gncTaxTableGetName (ttw->current_table));
            gnc_error_dialog (GTK_WINDOW(ttw->dialog), "%s", message);
        g_free (message);
        return;
    }

    tax_table_delete_request (ttw, TAX_TABLE_DELETE);
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

    if (g_list_length (gncTaxTableGetEntries (ttw->current_table)) <= 1)
    {
        char *message = _("You cannot remove the last entry from the tax table. "
                          "Try deleting the tax table if you want to do that.");
        gnc_error_dialog (GTK_WINDOW(ttw->dialog)  , "%s", message);
        return;
    }

    tax_table_delete_request (ttw, TAX_TABLE_ENTRY_DELETE);
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

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(ttw->dialog));
    gtk_window_destroy (GTK_WINDOW (ttw->dialog));
}

void
tax_table_window_close (GtkWidget *widget, gpointer data)
{
    TaxTableWindow *ttw = data;
    gnc_close_gui_component (ttw->component_id);
}

static gboolean
tax_table_window_close_request_cb (GtkWindow *window, G_GNUC_UNUSED gpointer user_data)
{
    // This callback allows the window size to be saved on closing with the X.
    gnc_save_window_size (GNC_PREFS_GROUP, window);
    return FALSE;
}

void
tax_table_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    TaxTableWindow *ttw = data;

    if (!ttw) return;

    gnc_unregister_gui_component (ttw->component_id);

    if (ttw->dialog)
        ttw->dialog = NULL;
    g_clear_object (&ttw->tables_selection);
    g_clear_object (&ttw->tables_model);
    g_clear_object (&ttw->entries_selection);
    g_clear_object (&ttw->entries_model);
    g_free (ttw);
}

static gboolean
tax_table_window_key_press_cb (GtkEventControllerKey *key, guint keyval,
                               guint keycode, GdkModifierType state,
                               gpointer user_data)
{
    TaxTableWindow *ttw = user_data;

    if (keyval == GDK_KEY_Escape)
    {
        tax_table_window_close_handler (ttw);
        return TRUE;
    }
    else
        return FALSE;
}

static gboolean
find_handler (gpointer find_data, gpointer data)
{
    TaxTableWindow *ttw = data;
    QofBook *book = find_data;

    return (ttw != NULL && ttw->book == book);
}

/* Create a tax-table window */
TaxTableWindow *
gnc_ui_tax_table_window_new (GtkWindow *parent, QofBook *book)
{
    TaxTableWindow *ttw;
    GtkBuilder *builder;
if (!book) return NULL;

    /*
     * Find an existing tax-table window.  If found, bring it to
     * the front.  If we have an actual owner, then set it in
     * the window.
     */
    ttw = gnc_find_first_gui_component (DIALOG_TAX_TABLE_CM_CLASS, find_handler,
                                        book);
    if (ttw)
    {
        gtk_window_present (GTK_WINDOW(ttw->dialog));
        return ttw;
    }

    /* Didn't find one -- create a new window */
    ttw = g_new0 (TaxTableWindow, 1);
    ttw->book = book;
    ttw->session = gnc_get_current_session ();

    /* Open and read the Glade File */
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-tax-table.glade", "tax_table_window");
    ttw->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "tax_table_window"));
    ttw->names_view = GTK_WIDGET(gtk_builder_get_object (builder, "tax_tables_view"));
    ttw->entries_view = GTK_WIDGET(gtk_builder_get_object (builder, "tax_table_entries"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(ttw->dialog), "gnc-id-new-tax-table");
    gnc_widget_style_context_add_class (GTK_WIDGET(ttw->dialog), "gnc-class-taxes");

    g_signal_connect (ttw->dialog, "close-request",
                      G_CALLBACK(tax_table_window_close_request_cb), ttw);

    GtkEventController *event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(ttw->dialog), event_controller);
    g_signal_connect (event_controller,
                      "key-pressed",
                      G_CALLBACK(tax_table_window_key_press_cb), ttw);

    /* Create GTK4 models for tax tables and their entries. The backing data is
     * sorted before insertion, preserving the old TreeModel ordering without
     * exposing mutable engine pointers as model properties. */
    ttw->tables_model = g_list_store_new (G_TYPE_OBJECT);
    ttw->tables_selection = GTK_SINGLE_SELECTION (gtk_single_selection_new
        (G_LIST_MODEL (g_object_ref (ttw->tables_model))));
    gtk_single_selection_set_autoselect (ttw->tables_selection, FALSE);
    gtk_column_view_set_model (GTK_COLUMN_VIEW (ttw->names_view),
                               GTK_SELECTION_MODEL (ttw->tables_selection));
    GtkColumnViewColumn *column = tax_table_column_new ("", "tax-table-name");
    gtk_column_view_append_column (GTK_COLUMN_VIEW (ttw->names_view), column);
    g_object_unref (column);
    g_signal_connect (ttw->tables_selection, "selection-changed",
                      G_CALLBACK (tax_table_selection_changed), ttw);

    ttw->entries_model = g_list_store_new (G_TYPE_OBJECT);
    ttw->entries_selection = GTK_SINGLE_SELECTION (gtk_single_selection_new
        (G_LIST_MODEL (g_object_ref (ttw->entries_model))));
    gtk_single_selection_set_autoselect (ttw->entries_selection, FALSE);
    gtk_column_view_set_model (GTK_COLUMN_VIEW (ttw->entries_view),
                               GTK_SELECTION_MODEL (ttw->entries_selection));
    column = tax_table_column_new (_("Account"), "tax-entry-name");
    gtk_column_view_append_column (GTK_COLUMN_VIEW (ttw->entries_view), column);
    g_object_unref (column);
    column = tax_table_column_new (_("Amount"), "tax-entry-amount");
    gtk_column_view_append_column (GTK_COLUMN_VIEW (ttw->entries_view), column);
    g_object_unref (column);
    g_signal_connect (ttw->entries_selection, "selection-changed",
                      G_CALLBACK (tax_table_entry_selection_changed), ttw);
    g_signal_connect (ttw->entries_view, "activate",
                      G_CALLBACK (tax_table_entry_activated), ttw);
    /* Setup signals */
gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ttw);

    /* register with component manager */
    ttw->component_id =
        gnc_register_gui_component (DIALOG_TAX_TABLE_CM_CLASS,
                                    tax_table_window_refresh_handler,
                                    tax_table_window_close_handler,
                                    ttw);

    gnc_gui_component_set_session (ttw->component_id, ttw->session);

    tax_table_window_refresh (ttw);
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(ttw->dialog), parent);
    gtk_window_present (GTK_WINDOW (ttw->dialog));

    g_object_unref (G_OBJECT(builder));

    return ttw;
}

typedef struct
{
    GtkWindow *window;
    QofBook *book;
    GtkEntry *name_entry;
    GtkDropDown *type_dropdown;
    GNCAmountEdit *amount_edit;
    GNCAccountSel *account_selector;
    GncTaxTableCreatedCB callback;
    gpointer user_data;
    gboolean completed;
} TaxTableCreateRequest;

static void
new_tax_table_async_update_amount_format (TaxTableCreateRequest *request)
{
    GNCPrintAmountInfo print_info;
    Account *account;

    if (gtk_drop_down_get_selected (request->type_dropdown) == 0)
    {
        account = gnc_account_sel_get_account (request->account_selector);
        if (account)
        {
            gnc_commodity *currency = xaccAccountGetCommodity (account);
            print_info = gnc_commodity_print_info (currency, FALSE);
            gnc_amount_edit_set_fraction (request->amount_edit,
                                          gnc_commodity_get_fraction (currency));
        }
        else
        {
            print_info = gnc_integral_print_info ();
            gnc_amount_edit_set_fraction (request->amount_edit, 100000);
        }
    }
    else
    {
        print_info = gnc_integral_print_info ();
        print_info.max_decimal_places = 5;
        gnc_amount_edit_set_fraction (request->amount_edit, 100000);
    }

    gnc_amount_edit_set_print_info (request->amount_edit, print_info);
}

static void
new_tax_table_async_type_changed (GObject *object, GParamSpec *pspec,
                                  gpointer user_data)
{
    (void)object;
    (void)pspec;
    new_tax_table_async_update_amount_format (user_data);
}

static void
new_tax_table_async_account_changed (GNCAccountSel *selector, gpointer user_data)
{
    (void)selector;
    new_tax_table_async_update_amount_format (user_data);
}

static void
new_tax_table_async_request_free (TaxTableCreateRequest *request)
{
    g_clear_object (&request->window);
    g_free (request);
}

static void
new_tax_table_async_complete (TaxTableCreateRequest *request,
                              GncTaxTable *table, gboolean accepted,
                              gboolean destroy_window)
{
    if (!request || request->completed)
        return;

    request->completed = TRUE;
    if (destroy_window)
        gtk_window_destroy (request->window);
    if (request->callback)
        request->callback (table, accepted, request->user_data);
    new_tax_table_async_request_free (request);
}

static gboolean
new_tax_table_async_close_request (GtkWindow *window, gpointer user_data)
{
    (void)window;
    new_tax_table_async_complete (user_data, NULL, FALSE, TRUE);
    return TRUE;
}

static void
new_tax_table_async_destroyed (GtkWidget *widget, gpointer user_data)
{
    TaxTableCreateRequest *request = user_data;

    (void)widget;
    new_tax_table_async_complete (request, NULL, FALSE, FALSE);
}

static void
new_tax_table_async_cancel_clicked (GtkButton *button, gpointer user_data)
{
    (void)button;
    new_tax_table_async_complete (user_data, NULL, FALSE, TRUE);
}

static void
new_tax_table_async_accept_clicked (GtkButton *button, gpointer user_data)
{
    TaxTableCreateRequest *request = user_data;
    const char *name;
    Account *account;
    gnc_numeric amount;
    GncTaxTable *table;
    GncTaxTableEntry *entry;
    GError *error = NULL;
    gint result;
    gint type;

    (void)button;
    if (qof_book_shutting_down (request->book))
    {
        new_tax_table_async_complete (request, NULL, FALSE, TRUE);
        return;
    }

    name = gtk_editable_get_text (GTK_EDITABLE (request->name_entry));
    if (!name || !*name)
    {
        gnc_error_dialog (request->window, "%s",
                          _("You must provide a name for this Tax Table."));
        return;
    }
    if (gncTaxTableLookupByName (request->book, name))
    {
        char *message = g_strdup_printf (
            _("You must provide a unique name for this Tax Table. "
              "Your choice \"%s\" is already in use."), name);
        gnc_error_dialog (request->window, "%s", message);
        g_free (message);
        return;
    }

    account = gnc_account_sel_get_account (request->account_selector);
    if (!account)
    {
        gnc_error_dialog (request->window, "%s", _("You must choose a Tax Account."));
        return;
    }

    result = gnc_amount_edit_expr_is_valid (request->amount_edit, &amount,
                                            TRUE, &error);
    if (result == 1)
    {
        gnc_error_dialog (request->window, "%s", error->message);
        g_clear_error (&error);
        return;
    }

    amount = gnc_amount_edit_get_amount (request->amount_edit);
    type = gtk_drop_down_get_selected (request->type_dropdown) == 0
        ? GNC_AMT_TYPE_VALUE : GNC_AMT_TYPE_PERCENT;
    if (type == GNC_AMT_TYPE_PERCENT &&
        gnc_numeric_compare (gnc_numeric_abs (amount),
                             gnc_numeric_create (100, 1)) > 0)
    {
        gnc_error_dialog (request->window, "%s",
                          _("Percentage amount must be between -100 and 100."));
        return;
    }

    gnc_suspend_gui_refresh ();
    table = gncTaxTableCreate (request->book);
    gncTaxTableBeginEdit (table);
    gncTaxTableSetName (table, name);
    entry = gncTaxTableEntryCreate ();
    gncTaxTableAddEntry (table, entry);
    gncTaxTableEntrySetAccount (entry, account);
    gncTaxTableEntrySetType (entry, type);
    gncTaxTableEntrySetAmount (entry, amount);
    gncTaxTableChanged (table);
    gncTaxTableCommitEdit (table);
    gnc_resume_gui_refresh ();

    new_tax_table_async_complete (request, table, TRUE, TRUE);
}

void
gnc_ui_tax_table_new_from_name_async (GtkWindow *parent, QofBook *book,
                                      const char *name,
                                      GncTaxTableCreatedCB callback,
                                      gpointer user_data)
{
    TaxTableCreateRequest *request;
    GtkWidget *content;
    GtkWidget *grid;
    GtkWidget *label;
    GtkWidget *button_box;
    GtkWidget *cancel_button;
    GtkWidget *accept_button;
    GtkStringList *types;

    if (!book || qof_book_shutting_down (book))
    {
        if (callback)
            callback (NULL, FALSE, user_data);
        return;
    }

    request = g_new0 (TaxTableCreateRequest, 1);
    request->book = book;
    request->callback = callback;
    request->user_data = user_data;
    request->window = GTK_WINDOW (g_object_ref_sink (gtk_window_new ()));
    gnc_window_bind_to_application (request->window);
    gtk_window_set_title (request->window, _("New Tax Table"));
    gtk_window_set_modal (request->window, TRUE);
    gtk_window_set_resizable (request->window, FALSE);
    if (parent)
    {
        gtk_window_set_transient_for (request->window, parent);
        g_signal_connect_object (parent, "destroy",
                                 G_CALLBACK (gtk_window_destroy), request->window,
                                 G_CONNECT_SWAPPED);
    }

    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    gtk_widget_set_margin_start (content, 18);
    gtk_widget_set_margin_end (content, 18);
    gtk_widget_set_margin_top (content, 18);
    gtk_widget_set_margin_bottom (content, 18);
    gtk_window_set_child (request->window, content);

    grid = gtk_grid_new ();
    gtk_grid_set_row_spacing (GTK_GRID (grid), 8);
    gtk_grid_set_column_spacing (GTK_GRID (grid), 12);
    gtk_box_append (GTK_BOX (content), grid);

    label = gtk_label_new_with_mnemonic (_("_Name"));
    gtk_widget_set_halign (label, GTK_ALIGN_END);
    gtk_grid_attach (GTK_GRID (grid), label, 0, 0, 1, 1);
    request->name_entry = GTK_ENTRY (gtk_entry_new ());
    gtk_editable_set_text (GTK_EDITABLE (request->name_entry), name ? name : "");
    gtk_widget_set_hexpand (GTK_WIDGET (request->name_entry), TRUE);
    gtk_grid_attach (GTK_GRID (grid), GTK_WIDGET (request->name_entry), 1, 0, 1, 1);
    gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (request->name_entry));

    label = gtk_label_new_with_mnemonic (_("_Type"));
    gtk_widget_set_halign (label, GTK_ALIGN_END);
    gtk_grid_attach (GTK_GRID (grid), label, 0, 1, 1, 1);
    types = gtk_string_list_new ((const char * const[]){ _("Value"), _("Percent"), NULL });
    request->type_dropdown = gnc_gtk_drop_down_new (G_LIST_MODEL (types), NULL);
    gtk_drop_down_set_selected (request->type_dropdown, 1);
    gtk_grid_attach (GTK_GRID (grid), GTK_WIDGET (request->type_dropdown), 1, 1, 1, 1);
    gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (request->type_dropdown));

    label = gtk_label_new_with_mnemonic (_("_Value"));
    gtk_widget_set_halign (label, GTK_ALIGN_END);
    gtk_grid_attach (GTK_GRID (grid), label, 0, 2, 1, 1);
    request->amount_edit = GNC_AMOUNT_EDIT (gnc_amount_edit_new ());
    gnc_amount_edit_set_evaluate_on_enter (request->amount_edit, TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET (request->amount_edit), TRUE);
    gtk_grid_attach (GTK_GRID (grid), GTK_WIDGET (request->amount_edit), 1, 2, 1, 1);
    gtk_label_set_mnemonic_widget (GTK_LABEL (label),
                                   gnc_amount_edit_gtk_entry (request->amount_edit));

    label = gtk_label_new_with_mnemonic (_("_Account"));
    gtk_widget_set_halign (label, GTK_ALIGN_END);
    gtk_widget_set_valign (label, GTK_ALIGN_START);
    gtk_grid_attach (GTK_GRID (grid), label, 0, 3, 1, 1);
    request->account_selector = GNC_ACCOUNT_SEL (gnc_account_sel_new ());
    gtk_widget_set_hexpand (GTK_WIDGET (request->account_selector), TRUE);
    gtk_grid_attach (GTK_GRID (grid), GTK_WIDGET (request->account_selector), 1, 3, 1, 1);
    gtk_label_set_mnemonic_widget (GTK_LABEL (label),
                                   GTK_WIDGET (request->account_selector));

    button_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_halign (button_box, GTK_ALIGN_END);
    cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
    accept_button = gtk_button_new_with_mnemonic (_("_OK"));
    gtk_box_append (GTK_BOX (button_box), cancel_button);
    gtk_box_append (GTK_BOX (button_box), accept_button);
    gtk_box_append (GTK_BOX (content), button_box);
    gtk_window_set_default_widget (request->window, accept_button);

    g_signal_connect (request->type_dropdown, "notify::selected",
                      G_CALLBACK (new_tax_table_async_type_changed), request);
    g_signal_connect (request->account_selector, "account_sel_changed",
                      G_CALLBACK (new_tax_table_async_account_changed), request);
    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (new_tax_table_async_cancel_clicked), request);
    g_signal_connect (accept_button, "clicked",
                      G_CALLBACK (new_tax_table_async_accept_clicked), request);
    g_signal_connect (request->window, "close-request",
                      G_CALLBACK (new_tax_table_async_close_request), request);
    g_signal_connect (request->window, "destroy",
                      G_CALLBACK (new_tax_table_async_destroyed), request);

    new_tax_table_async_update_amount_format (request);
    gtk_widget_grab_focus (GTK_WIDGET (request->name_entry));
    gtk_window_present (request->window);
}
