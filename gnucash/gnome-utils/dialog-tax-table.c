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
#include "qofevent.h"
#include "gnc-amount-edit.h"
#include "gnc-tree-view-account.h"

#include "gncTaxTable.h"
#include "dialog-tax-table.h"

#define DIALOG_TAX_TABLE_CM_CLASS "tax-table-dialog"
#define GNC_PREFS_GROUP "dialogs.business.tax-tables"

enum tax_table_cols
{
    TAX_TABLE_COL_NAME = 0,
    TAX_TABLE_COL_POINTER,
    NUM_TAX_TABLE_COLS
};

enum tax_entry_cols
{
    TAX_ENTRY_COL_NAME = 0,
    TAX_ENTRY_COL_POINTER,
    TAX_ENTRY_COL_AMOUNT,
    NUM_TAX_ENTRY_COLS
};

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

    GncTaxTable      *current_table;
    GncTaxTableEntry *current_entry;
    QofBook          *book;
    gint              component_id;
    QofSession       *session;
    guint             ref_count;
    gboolean          closing;
};

typedef struct _new_taxtable
{
    GtkWidget *dialog;
    GtkWidget *name_entry;
    GtkWidget *amount_entry;
    GtkWidget *acct_tree;

    GncTaxTable      *created_table;
    TaxTableWindow   *ttw;
    GncTaxTableEntry *entry;
    gint              type;
    gboolean          new_table;
    gboolean          asynchronous;
    guint             ref_count;
    GWeakRef          parent;
    gboolean          has_parent;
    GWeakRef          owner_parent;
    gboolean          has_owner_parent;
    gboolean          owner_destroyed;
    gulong            owner_destroy_handler;
    QofBook          *book;
    QofSession       *session_identity;
    GncGUID           book_guid;
    GncGUID           table_guid;
    GncGUID           entry_account_guid;
    GncAmountType     entry_type;
    gnc_numeric       entry_amount;
    GncTaxTableEntry *entry_identity;
    gboolean          has_table;
    gboolean          has_entry;
    gboolean          responding;
    gulong            response_handler;
    GncTaxTableCreateCallback create_callback;
    gpointer          create_callback_data;
    GncGUID           created_table_guid;
    gboolean          has_created_table;
    gboolean          parent_destroyed;
    gulong            parent_destroy_handler;
    guint             event_handler;
    gboolean          stale_table;
} NewTaxTable;

static gboolean new_tax_table_resolve_context (NewTaxTable *ntt);

static TaxTableWindow *
tax_table_window_ref (TaxTableWindow *ttw)
{
    ++ttw->ref_count;
    return ttw;
}

static void
tax_table_window_unref (TaxTableWindow *ttw)
{
    if (--ttw->ref_count == 0)
        g_free (ttw);
}

static void
new_tax_table_free (NewTaxTable *ntt)
{
    if (--ntt->ref_count != 0)
        return;
    if (ntt->book)
        g_object_remove_weak_pointer (G_OBJECT (ntt->book),
                                      (gpointer *)&ntt->book);
    if (ntt->event_handler)
        qof_event_unregister_handler (ntt->event_handler);
    if (ntt->has_parent)
    {
        GtkWidget *parent = g_weak_ref_get (&ntt->parent);
        if (parent)
        {
            if (ntt->parent_destroy_handler &&
                g_signal_handler_is_connected (parent,
                                               ntt->parent_destroy_handler))
                g_signal_handler_disconnect (parent,
                                             ntt->parent_destroy_handler);
            if (g_object_get_data (G_OBJECT (parent), "tax-entry-dialog-pending") == ntt)
                g_object_set_data (G_OBJECT (parent), "tax-entry-dialog-pending", NULL);
            g_object_unref (parent);
        }
        g_weak_ref_clear (&ntt->parent);
    }
    if (ntt->has_owner_parent)
    {
        GtkWidget *owner = g_weak_ref_get (&ntt->owner_parent);
        if (owner)
        {
            if (ntt->owner_destroy_handler &&
                g_signal_handler_is_connected (owner,
                                               ntt->owner_destroy_handler))
                g_signal_handler_disconnect (owner, ntt->owner_destroy_handler);
            g_object_unref (owner);
        }
        g_weak_ref_clear (&ntt->owner_parent);
    }
    if (ntt->asynchronous && ntt->ttw)
        tax_table_window_unref (ntt->ttw);
    g_free (ntt);
}

static void
new_tax_table_parent_destroyed ([[maybe_unused]] GtkWidget *parent,
                                NewTaxTable *ntt)
{
    ntt->parent_destroyed = TRUE;
}

static void
new_tax_table_owner_destroyed ([[maybe_unused]] GtkWidget *owner,
                               NewTaxTable *ntt)
{
    ntt->owner_destroyed = TRUE;
    if (ntt->dialog && !gtk_widget_in_destruction (ntt->dialog))
        gtk_widget_destroy (ntt->dialog);
}

static void
new_tax_table_create_complete (NewTaxTable *ntt)
{
    GtkWidget *parent = NULL;
    GtkWidget *owner = NULL;
    GncTaxTable *table = NULL;
    gboolean context_valid = FALSE;
    GncTaxTableCreateCallback callback = ntt->create_callback;
    gpointer user_data = ntt->create_callback_data;

    if (!callback)
        return;

    /* Clear first: callback code may destroy the parent and re-enter the
     * dialog's destroy path. */
    ntt->create_callback = NULL;
    ntt->create_callback_data = NULL;
    if (!ntt->parent_destroyed && !ntt->owner_destroyed && ntt->book &&
        gnc_current_session_exist () &&
        gnc_get_current_session () == ntt->session_identity &&
        qof_session_get_book (ntt->session_identity) == ntt->book &&
        qof_book_is_open (ntt->book) && !qof_book_shutting_down (ntt->book) &&
        !qof_book_is_readonly (ntt->book) &&
        guid_equal (qof_book_get_guid (ntt->book), &ntt->book_guid))
    {
        parent = g_weak_ref_get (&ntt->parent);
        context_valid = parent && !gtk_widget_in_destruction (parent) &&
                        ntt->ttw && !ntt->ttw->closing &&
                        ntt->ttw->dialog == parent;
        if (context_valid && ntt->has_created_table)
            table = gncTaxTableLookup (ntt->book, &ntt->created_table_guid);
        if (context_valid && ntt->has_owner_parent)
        {
            owner = g_weak_ref_get (&ntt->owner_parent);
            if (!owner || gtk_widget_in_destruction (owner))
                g_clear_object (&owner);
        }
        if (context_valid && ntt->has_owner_parent && !owner)
            table = NULL;
        if (!context_valid)
            g_clear_object (&parent);
    }
    callback (ntt->has_owner_parent && owner ? GTK_WINDOW (owner) : NULL,
              table, user_data);
    g_clear_object (&owner);
    g_clear_object (&parent);
}

static void
new_tax_table_target_event (QofInstance *entity, QofEventId event_type,
                            gpointer user_data, [[maybe_unused]] gpointer event_data)
{
    NewTaxTable *ntt = user_data;
    if (!ntt->has_table || ntt->stale_table || !ntt->book ||
        !(event_type & (QOF_EVENT_MODIFY | QOF_EVENT_DESTROY)) ||
        !guid_equal (qof_instance_get_guid (entity), &ntt->table_guid))
        return;
    ntt->stale_table = TRUE;
}

static void
new_tax_table_show_error (NewTaxTable *ntt, const char *message)
{
    gnc_error_dialog_async (GTK_WINDOW (ntt->dialog), "%s", message);
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
        Account *acc = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(ntt->acct_tree));
        gnc_commodity *currency = xaccAccountGetCommodity (acc);
        print_info = gnc_commodity_print_info (currency, FALSE);
        gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT(ntt->amount_entry),
                                      gnc_commodity_get_fraction (currency));
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
    GncGUID account_guid;
    QofBook *book;
    GncTaxTable *table;
    g_autofree char *name_copy = NULL;
    gint input_type;

    g_return_val_if_fail (ntt, FALSE);
    ttw = ntt->ttw;
    book = ttw->book;

    /* Verify that we've got real, valid data */

    /* verify the name, maybe */
    if (ntt->new_table)
    {
        name_copy = g_strdup (gtk_entry_get_text (GTK_ENTRY(ntt->name_entry)));
        name = name_copy;
        if (name == NULL || *name == '\0')
        {
            message = _("You must provide a name for this Tax Table.");
            new_tax_table_show_error (ntt, message);
            return FALSE;
        }
        if (gncTaxTableLookupByName (ttw->book, name))
        {
            message = g_strdup_printf (_(
                                          "You must provide a unique name for this Tax Table. "
                                          "Your choice \"%s\" is already in use."), name);
            new_tax_table_show_error (ntt, message);
            g_free (message);
            return FALSE;
        }
    }

    /* test for valid value */
    if (!new_tax_table_check_entry (ntt, &error))
    {
        message = g_strdup (error->message);
        new_tax_table_show_error (ntt, message);
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
        new_tax_table_show_error (ntt, message);
        return FALSE;
    }

    /* verify the account */
    acc = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(ntt->acct_tree));
    if (acc == NULL)
    {
        message = _("You must choose a Tax Account.");
        new_tax_table_show_error (ntt, message);
        return FALSE;
    }

    account_guid = *qof_instance_get_guid (QOF_INSTANCE (acc));
    input_type = ntt->type;
    if (ntt->asynchronous && !new_tax_table_resolve_context (ntt))
        return FALSE;
    book = ntt->ttw->book;
    acc = xaccAccountLookup (&account_guid, book);
    if (!acc)
        return FALSE;

    if (ntt->event_handler)
    {
        qof_event_unregister_handler (ntt->event_handler);
        ntt->event_handler = 0;
    }

    gnc_suspend_gui_refresh ();

    /* Ok, it's all valid, now either change to add this thing */
    if (ntt->new_table)
    {
        table = gncTaxTableCreate (book);
        ttw->current_table = table;
        ntt->created_table = table;
        gncTaxTableBeginEdit (table);
        gncTaxTableSetName (table, name_copy);
    }
    else
    {
        table = ttw->current_table;
        gncTaxTableBeginEdit (table);
    }

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
            gncTaxTableAddEntry (table, entry);
            ttw->current_entry = entry;
        }

        gncTaxTableEntrySetAccount (entry, acc);
        gncTaxTableEntrySetType (entry, input_type);
        gncTaxTableEntrySetAmount (entry, amount);
    }

    /* Mark the table as changed and commit it */
    gncTaxTableChanged (table);
    gncTaxTableCommitEdit (table);

    gnc_resume_gui_refresh ();
    return TRUE;
}

static void
combo_changed (GtkWidget *widget, NewTaxTable *ntt)
{
    gint index;

    g_return_if_fail (GTK_IS_COMBO_BOX(widget));
    g_return_if_fail (ntt);

    index = gtk_combo_box_get_active (GTK_COMBO_BOX(widget));
    ntt->type = index + 1;

    new_tax_table_check_entry (ntt, NULL);
}

static void
tax_table_account_selection_changed_cb (GtkTreeSelection *treeselection,
                                        NewTaxTable *ntt)
{
    new_tax_table_check_entry (ntt, NULL);
}

static gboolean
new_tax_table_resolve_context (NewTaxTable *ntt)
{
    GtkWidget *parent = g_weak_ref_get (&ntt->parent);
    QofSession *session;
    TaxTableWindow *ttw;
    GncTaxTable *table = NULL;
    GncTaxTableEntry *entry = NULL;

    if (!parent || gtk_widget_in_destruction (parent) || !ntt->book ||
        ntt->stale_table ||
        !gnc_current_session_exist () || qof_book_shutting_down (ntt->book) ||
        !qof_book_is_open (ntt->book) || qof_book_is_readonly (ntt->book) ||
        !guid_equal (qof_book_get_guid (ntt->book), &ntt->book_guid))
        goto fail;
    session = gnc_get_current_session ();
    if (!session || session != ntt->session_identity ||
        qof_session_get_book (session) != ntt->book)
        goto fail;
    ttw = g_object_get_data (G_OBJECT (parent), "dialog_info");
    if (!ttw || ttw != ntt->ttw || ttw->closing || ttw->dialog != parent ||
        ttw->book != ntt->book)
        goto fail;
    if (ntt->has_table)
    {
        table = gncTaxTableLookup (ntt->book, &ntt->table_guid);
        if (!table)
            goto fail;
    }
    if (ntt->has_entry)
    {
        gboolean found = FALSE;
        GList *entries = gncTaxTableGetEntries (table);
        for (GList *node = entries; node; node = node->next)
        {
            GncTaxTableEntry *candidate = node->data;
            if (candidate != ntt->entry_identity)
                continue;
            Account *account = gncTaxTableEntryGetAccount (candidate);
            if (account &&
                guid_equal (qof_instance_get_guid (QOF_INSTANCE (account)),
                            &ntt->entry_account_guid) &&
                gncTaxTableEntryGetType (candidate) == ntt->entry_type &&
                gnc_numeric_compare (gncTaxTableEntryGetAmount (candidate),
                                     ntt->entry_amount) == 0)
            {
                entry = candidate;
                found = TRUE;
            }
            break;
        }
        if (!found)
            goto fail;
    }
    ttw->current_table = table ? table : ttw->current_table;
    if (entry)
        ttw->current_entry = entry;
    ntt->ttw = ttw;
    ntt->entry = entry;
    g_object_unref (parent);
    return TRUE;

fail:
    g_clear_object (&parent);
    return FALSE;
}

static void
new_tax_table_destroy_cb (GtkWidget *dialog, gpointer user_data)
{
    NewTaxTable *ntt = user_data;
    g_signal_handlers_disconnect_by_data (dialog, ntt);
    new_tax_table_create_complete (ntt);
    new_tax_table_free (ntt);
}

static void
new_tax_table_response_cb (GtkDialog *dialog, gint response, gpointer user_data)
{
    NewTaxTable *ntt = user_data;
    if (ntt->responding)
        return;
    ntt->responding = TRUE;
    g_object_ref (dialog);
    ++ntt->ref_count;
    if (response != GTK_RESPONSE_OK)
    {
        if (g_signal_handler_is_connected (dialog, ntt->response_handler))
            g_signal_handler_disconnect (dialog, ntt->response_handler);
        gtk_widget_destroy (GTK_WIDGET (dialog));
    }
    else if (!new_tax_table_resolve_context (ntt))
    {
        if (g_signal_handler_is_connected (dialog, ntt->response_handler))
            g_signal_handler_disconnect (dialog, ntt->response_handler);
        gtk_widget_destroy (GTK_WIDGET (dialog));
    }
    else if (new_tax_table_ok_cb (ntt))
    {
        if (ntt->create_callback && ntt->created_table)
        {
            ntt->created_table_guid = *gncTaxTableGetGUID (ntt->created_table);
            ntt->has_created_table = TRUE;
            /* The callback request carries only a stable identifier after
             * this point; never retain the engine object across the destroy
             * and response callbacks. */
            ntt->created_table = NULL;
        }
        if (g_signal_handler_is_connected (dialog, ntt->response_handler))
            g_signal_handler_disconnect (dialog, ntt->response_handler);
        gtk_widget_destroy (GTK_WIDGET (dialog));
    }
    if (g_signal_handler_is_connected (dialog, ntt->response_handler))
        ntt->responding = FALSE;
    new_tax_table_free (ntt);
    g_object_unref (dialog);
}

static GncTaxTable *
new_tax_table_dialog (TaxTableWindow *ttw, gboolean new_table,
                      GncTaxTableEntry *entry, const char *name,
                      gboolean asynchronous,
                      GtkWindow *owner_parent,
                      GncTaxTableCreateCallback create_callback,
                      gpointer callback_data,
                      gboolean *async_started)
{
    NewTaxTable *ntt;
    GtkBuilder *builder;
    GtkWidget *box, *widget, *combo;
    gint index;
    GtkTreeSelection *selection;

    if (!ttw) return NULL;
    if (new_table && entry) return NULL;

    ntt = g_new0 (NewTaxTable, 1);
    ntt->ttw = ttw;
    ntt->entry = entry;
    ntt->new_table = new_table;
    ntt->asynchronous = asynchronous;
    ntt->create_callback = create_callback;
    ntt->create_callback_data = callback_data;
    ntt->ref_count = 1;
    if (asynchronous)
    {
        if (g_object_get_data (G_OBJECT (ttw->dialog), "tax-entry-dialog-pending"))
        {
            g_free (ntt);
            return NULL;
        }
        ntt->ttw = tax_table_window_ref (ttw);
        ntt->book = ttw->book;
        ntt->session_identity = ttw->session;
        ntt->book_guid = *qof_book_get_guid (ttw->book);
        if (!new_table && ttw->current_table)
        {
            ntt->has_table = TRUE;
            ntt->table_guid = *gncTaxTableGetGUID (ttw->current_table);
            ntt->event_handler = qof_event_register_handler (
                new_tax_table_target_event, ntt);
        }
        if (entry)
        {
            ntt->has_entry = TRUE;
            Account *account = gncTaxTableEntryGetAccount (entry);
            if (!account)
            {
                tax_table_window_unref (ntt->ttw);
                g_free (ntt);
                return NULL;
            }
            ntt->entry_account_guid = *qof_instance_get_guid (QOF_INSTANCE (account));
            ntt->entry_type = gncTaxTableEntryGetType (entry);
            ntt->entry_amount = gncTaxTableEntryGetAmount (entry);
            ntt->entry_identity = entry;
        }
        g_object_add_weak_pointer (G_OBJECT (ntt->book),
                                   (gpointer *)&ntt->book);
        g_weak_ref_init (&ntt->parent, G_OBJECT (ttw->dialog));
        ntt->has_parent = TRUE;
        if (owner_parent)
        {
            g_weak_ref_init (&ntt->owner_parent, G_OBJECT (owner_parent));
            ntt->has_owner_parent = TRUE;
            ntt->owner_destroy_handler = g_signal_connect (
                owner_parent, "destroy",
                G_CALLBACK (new_tax_table_owner_destroyed), ntt);
        }
        if (create_callback)
            ntt->parent_destroy_handler = g_signal_connect (
                ttw->dialog, "destroy",
                G_CALLBACK (new_tax_table_parent_destroyed), ntt);
        g_object_set_data (G_OBJECT (ttw->dialog), "tax-entry-dialog-pending", ntt);
    }

    if (entry)
        ntt->type = gncTaxTableEntryGetType (entry);
    else
        ntt->type = GNC_AMT_TYPE_PERCENT;

    /* Open and read the Glade File */
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-tax-table.glade", "type_liststore");
    gnc_builder_add_from_file (builder, "dialog-tax-table.glade", "new_tax_table_dialog");

    ntt->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "new_tax_table_dialog"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(ntt->dialog), "gnc-id-tax-table");
    gnc_widget_style_context_add_class (GTK_WIDGET(ntt->dialog), "gnc-class-taxes");

    ntt->name_entry = GTK_WIDGET(gtk_builder_get_object (builder, "name_entry"));
    if (name)
        gtk_entry_set_text (GTK_ENTRY(ntt->name_entry), name);

    /* Create the menu */
    combo = GTK_WIDGET(gtk_builder_get_object (builder, "type_combobox"));
    index = ntt->type ? ntt->type : GNC_AMT_TYPE_VALUE;
    gtk_combo_box_set_active (GTK_COMBO_BOX(combo), index - 1);
    g_signal_connect (combo, "changed", G_CALLBACK(combo_changed), ntt);

    /* Attach our own widgets */
    box = GTK_WIDGET(gtk_builder_get_object (builder, "amount_box"));
    ntt->amount_entry = widget = gnc_amount_edit_new ();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT(widget), TRUE);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT(widget), 100000);
    gtk_box_pack_start (GTK_BOX(box), widget, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "acct_window"));
    ntt->acct_tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    gtk_container_add (GTK_CONTAINER(box), ntt->acct_tree);
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(ntt->acct_tree), FALSE);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(ntt->acct_tree));
    g_signal_connect (G_OBJECT(selection), "changed",
                      G_CALLBACK(tax_table_account_selection_changed_cb), ntt);

    /* Make 'enter' do the right thing */
    gtk_entry_set_activates_default (GTK_ENTRY(gnc_amount_edit_gtk_entry
                                    (GNC_AMOUNT_EDIT(ntt->amount_entry))),
                                    TRUE);

    /* Fix mnemonics for generated target widgets */
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "value_label"));
    gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT(ntt->amount_entry), widget);
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "account_label"));
    gtk_label_set_mnemonic_widget (GTK_LABEL(widget), ntt->acct_tree);

    /* Fill in the widgets appropriately */
    if (entry)
    {
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(ntt->amount_entry),
                                    gncTaxTableEntryGetAmount (entry));
        gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT(ntt->acct_tree),
                gncTaxTableEntryGetAccount (entry));
    }

    /* Set our parent */
    gtk_window_set_transient_for (GTK_WINDOW(ntt->dialog), GTK_WINDOW(ttw->dialog));

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ntt);

    if (asynchronous)
    {
        gtk_window_set_destroy_with_parent (GTK_WINDOW (ntt->dialog), TRUE);
        ntt->response_handler = g_signal_connect (
            ntt->dialog, "response",
            G_CALLBACK (new_tax_table_response_cb), ntt);
        g_signal_connect (ntt->dialog, "destroy",
                          G_CALLBACK (new_tax_table_destroy_cb), ntt);
    }

    /* Configure visibility and focus before showing: show signals may
     * destroy the parent and complete/free this request re-entrantly. */
    if (new_table == FALSE)
    {
        gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (builder, "table_title")));
        gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (builder, "table_name")));
        gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (builder, "spacer")));
        gtk_widget_hide (ntt->name_entry);
        /* Tables are great for layout, but a pain when you hide widgets */
        GTK_WIDGET(gtk_builder_get_object (builder, "ttd_table"));
        gtk_widget_grab_focus (gnc_amount_edit_gtk_entry
                               (GNC_AMOUNT_EDIT(ntt->amount_entry)));
    }
    else
        gtk_widget_grab_focus (ntt->name_entry);

    if (async_started)
        *async_started = TRUE;
    gtk_widget_show_all (ntt->dialog);
    g_object_unref (G_OBJECT (builder));
    return NULL;
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

    view = GTK_TREE_VIEW(ttw->entries_view);
    store = GTK_LIST_STORE(gtk_tree_view_get_model (view));

    /* Clear the list */
    selected_entry = ttw->current_entry;
    gtk_list_store_clear (store);
    if (ttw->current_table == NULL)
        return;

    /* Add the items to the list */
    list = gncTaxTableGetEntries (ttw->current_table);
    if (list)
        list = g_list_reverse (g_list_copy (list));

    for (node = list ; node; node = node->next)
    {
        char *row_text[3];
        GncTaxTableEntry *entry = node->data;
        Account *acc = gncTaxTableEntryGetAccount (entry);
        gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

        row_text[0] = gnc_account_get_full_name (acc);
        switch (gncTaxTableEntryGetType (entry))
        {
        case GNC_AMT_TYPE_PERCENT:
            row_text[1] =
                g_strdup_printf ("%s%%",
                                 xaccPrintAmount (amount,
                                                  gnc_default_print_info (FALSE)));
            break;
        case GNC_AMT_TYPE_VALUE:
            row_text[1] =
                g_strdup_printf ("%s",
                                 xaccPrintAmount (amount,
                                                  gnc_default_print_info (TRUE)));
            break;
         default:
             row_text[1] = NULL;
             break;
        }

        gtk_list_store_prepend (store, &iter);
        gtk_list_store_set (store, &iter,
                            TAX_ENTRY_COL_NAME, row_text[0],
                            TAX_ENTRY_COL_POINTER, entry,
                            TAX_ENTRY_COL_AMOUNT, row_text[1],
                            -1);
        if (entry == selected_entry)
        {
            path = gtk_tree_model_get_path (GTK_TREE_MODEL(store), &iter);
            reference = gtk_tree_row_reference_new (GTK_TREE_MODEL(store), path);
            gtk_tree_path_free (path);
        }

        g_free (row_text[0]);
        g_free (row_text[1]);
    }

    if (list)
        g_list_free (list);

    if (reference)
    {
        path = gtk_tree_row_reference_get_path (reference);
        gtk_tree_row_reference_free (reference);
        if (path)
        {
            selection = gtk_tree_view_get_selection (view);
            gtk_tree_selection_select_path (selection, path);
            gtk_tree_view_scroll_to_cell (view, path, NULL, TRUE, 0.5, 0.0);
            gtk_tree_path_free (path);
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
    GncTaxTable *saved_current_table = ttw->current_table;

    g_return_if_fail (ttw);
    view = GTK_TREE_VIEW(ttw->names_view);
    store = GTK_LIST_STORE(gtk_tree_view_get_model (view));

    /* Clear the list */
    gtk_list_store_clear(store);

    gnc_gui_component_clear_watches (ttw->component_id);

    /* Add the items to the list */
    list = gncTaxTableGetTables (ttw->book);
    if (list)
        list = g_list_reverse (g_list_copy (list));

    for (node = list; node; node = node->next)
    {
        GncTaxTable *table = node->data;

        gnc_gui_component_watch_entity (ttw->component_id,
                                        gncTaxTableGetGUID (table),
                                        QOF_EVENT_MODIFY);

        gtk_list_store_prepend (store, &iter);
        gtk_list_store_set (store, &iter,
                            TAX_TABLE_COL_NAME, gncTaxTableGetName (table),
                            TAX_TABLE_COL_POINTER, table,
                            -1);

        if (table == saved_current_table)
        {
            path = gtk_tree_model_get_path (GTK_TREE_MODEL(store), &iter);
            reference = gtk_tree_row_reference_new (GTK_TREE_MODEL(store), path);
            gtk_tree_path_free (path);
        }
    }

    if (list)
        g_list_free (list);

    gnc_gui_component_watch_entity_type (ttw->component_id,
                                         GNC_TAXTABLE_MODULE_NAME,
                                         QOF_EVENT_CREATE | QOF_EVENT_DESTROY);

    if (reference)
    {
        path = gtk_tree_row_reference_get_path (reference);
        gtk_tree_row_reference_free (reference);
        if (path)
        {
            selection = gtk_tree_view_get_selection (view);
            gtk_tree_selection_select_path (selection, path);
            gtk_tree_view_scroll_to_cell (view, path, NULL, TRUE, 0.5, 0.0);
            gtk_tree_path_free (path);
        }
    }

    tax_table_entries_refresh (ttw);
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

    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
        return;

    gtk_tree_model_get (model, &iter, TAX_TABLE_COL_POINTER, &table, -1);
    g_return_if_fail (table);

    /* If we've changed, then reset the entry list */
    if (table != ttw->current_table)
    {
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

    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        ttw->current_entry = NULL;
        return;
    }

    gtk_tree_model_get (model, &iter, TAX_ENTRY_COL_POINTER, &ttw->current_entry, -1);
}

static void
tax_table_entry_row_activated (GtkTreeView       *tree_view,
                               GtkTreePath       *path,
                               GtkTreeViewColumn *column,
                               gpointer           user_data)
{
    TaxTableWindow *ttw = user_data;

    new_tax_table_dialog (ttw, FALSE, ttw->current_entry, NULL, TRUE,
                          NULL, NULL, NULL, NULL);
}

void
tax_table_new_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
    g_return_if_fail (ttw);
    new_tax_table_dialog (ttw, TRUE, NULL, NULL, TRUE, NULL, NULL, NULL, NULL);
}


typedef struct
{
    GtkWidget *entry;
    GWeakRef parent;
    QofBook *book; /* weak: cleared when the book is finalized */
    QofSession *session_identity;
    GncGUID book_guid;
    GncGUID table_guid;
    gboolean parent_destroyed;
} RenameTaxTable;

static void
rename_tax_table_parent_destroy_cb (GtkWidget *parent, gpointer user_data)
{
    RenameTaxTable *rename = user_data;
    rename->parent_destroyed = TRUE;
}

static void
rename_tax_table_request_free (RenameTaxTable *rename)
{
    GtkWidget *parent = g_weak_ref_get (&rename->parent);
    if (parent)
    {
        g_signal_handlers_disconnect_by_data (parent, rename);
        g_object_unref (parent);
    }
    if (rename->book)
        g_object_remove_weak_pointer (G_OBJECT (rename->book),
                                      (gpointer *)&rename->book);
    g_weak_ref_clear (&rename->parent);
    g_free (rename);
}

static void
rename_tax_table_destroy_cb (GtkWidget *dialog, gpointer user_data)
{
    rename_tax_table_request_free (user_data);
}

static void
rename_tax_table_response_cb (GtkDialog *dialog, gint response,
                              gpointer user_data)
{
    RenameTaxTable *rename = user_data;
    GtkWidget *parent = g_weak_ref_get (&rename->parent);
    QofSession *session_identity = rename->session_identity;
    GncGUID book_guid = rename->book_guid;
    GncGUID table_guid = rename->table_guid;
    char *newname = response == GTK_RESPONSE_OK ?
        g_strdup (gtk_entry_get_text (GTK_ENTRY (rename->entry))) : NULL;

    /* Keep the request alive while destroying the prompt: its weak book
       pointer must still be clearable if destruction reenters engine events. */
    g_signal_handlers_disconnect_by_data (dialog, rename);
    gtk_widget_destroy (GTK_WIDGET (dialog));

    if (response != GTK_RESPONSE_OK || !parent || !newname || !*newname ||
        rename->parent_destroyed || gtk_widget_in_destruction (parent))
        goto cleanup;

    /* Engine events may have changed the selection or book while the prompt
       was open. Require the original session and weak book to still be live,
       open, writable, and identical before resolving the table again. */
    if (!rename->book || !gnc_current_session_exist ())
        goto cleanup;
    QofSession *session = gnc_get_current_session ();
    if (!session || session != session_identity ||
        qof_session_get_book (session) != rename->book ||
        qof_book_shutting_down (rename->book) ||
        !qof_book_is_open (rename->book) ||
        qof_book_is_readonly (rename->book) ||
        !guid_equal (qof_book_get_guid (rename->book), &book_guid))
        goto cleanup;

    GncTaxTable *table = gncTaxTableLookup (rename->book, &table_guid);
    if (!table)
        goto cleanup;

    const char *current_name = gncTaxTableGetName (table);
    if (g_strcmp0 (current_name, newname) == 0)
        goto cleanup;

    GncTaxTable *conflict = gncTaxTableLookupByName (rename->book, newname);
    if (conflict && conflict != table)
    {
        char *message = g_strdup_printf (_("Tax table name \"%s\" already exists."),
                                         newname);
        gnc_error_dialog_async (GTK_WINDOW (parent), "%s", message);
        g_free (message);
    }
    else
        gncTaxTableSetName (table, newname);

cleanup:
    g_clear_object (&parent);
    g_free (newname);
    rename_tax_table_request_free (rename);
}

static void
rename_tax_table_dialog (GtkWidget *parent, QofSession *session,
                         QofBook *book,
                         GncTaxTable *table, const char *title,
                         const char *msg, const char *button_name,
                         const char *text)
{
    GtkWidget *vbox;
    GtkWidget *main_vbox;
    GtkWidget *label;
    GtkWidget *textbox;
    GtkWidget *dialog;
    GtkWidget *dvbox;
    RenameTaxTable *rename;

    main_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
    gtk_box_set_homogeneous (GTK_BOX(main_vbox), FALSE);
    gtk_container_set_border_width (GTK_CONTAINER(main_vbox), 6);
    gtk_widget_show (main_vbox);

    label = gtk_label_new (msg);
    gtk_label_set_justify (GTK_LABEL(label), GTK_JUSTIFY_LEFT);
    gtk_box_pack_start (GTK_BOX(main_vbox), label, FALSE, FALSE, 0);
    gtk_widget_show (label);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
    gtk_box_set_homogeneous (GTK_BOX(vbox), TRUE);
    gtk_container_set_border_width (GTK_CONTAINER(vbox), 6);
    gtk_container_add (GTK_CONTAINER(main_vbox), vbox);
    gtk_widget_show (vbox);

    textbox = gtk_entry_new ();
    gtk_widget_show (textbox);
    gtk_entry_set_text (GTK_ENTRY(textbox), text);
    gtk_box_pack_start (GTK_BOX(vbox), textbox, FALSE, FALSE, 0);

    dialog = gtk_dialog_new_with_buttons (title, GTK_WINDOW(parent),
                                          GTK_DIALOG_DESTROY_WITH_PARENT,
                                          _("_Cancel"), GTK_RESPONSE_CANCEL,
                                          button_name, GTK_RESPONSE_OK,
                                          NULL);
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_OK);

    gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);

    rename = g_new0 (RenameTaxTable, 1);
    rename->entry = textbox;
    rename->book = book;
    rename->session_identity = session;
    rename->book_guid = *qof_book_get_guid (book);
    rename->table_guid = *gncTaxTableGetGUID (table);
    g_weak_ref_init (&rename->parent, G_OBJECT (parent));
    g_object_add_weak_pointer (G_OBJECT (book), (gpointer *)&rename->book);
    g_signal_connect (parent, "destroy",
                      G_CALLBACK (rename_tax_table_parent_destroy_cb), rename);
    g_signal_connect (dialog, "response",
                      G_CALLBACK (rename_tax_table_response_cb), rename);
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (rename_tax_table_destroy_cb), rename);

    dvbox = gtk_dialog_get_content_area (GTK_DIALOG(dialog));
    gtk_box_pack_start (GTK_BOX(dvbox), main_vbox, TRUE, TRUE, 0);
    gtk_widget_show_all (dialog);
    gtk_widget_grab_focus (textbox);
}

void
tax_table_rename_table_cb (GtkButton *button, TaxTableWindow *ttw)
{
    const char *oldname;
    g_return_if_fail (ttw);

    if (!ttw->current_table)
        return;

    oldname = gncTaxTableGetName (ttw->current_table);
    rename_tax_table_dialog (ttw->dialog, ttw->session, ttw->book,
                             ttw->current_table,
                             _("Rename"), _("Please enter new name"),
                             _("_Rename"), oldname);
}

typedef struct
{
    TaxTableWindow *ttw;
    GncGUID table_guid;
    GncTaxTableEntry *entry_identity;
    gboolean delete_entry;
} TaxTableDeleteRequest;

static void
tax_table_delete_confirmed (GtkWindow *parent, gint response, gpointer user_data)
{
    TaxTableDeleteRequest *request = user_data;
    TaxTableWindow *ttw = request->ttw;
    GncTaxTable *table = NULL;

    if (response == GTK_RESPONSE_YES && parent && !ttw->closing &&
        ttw->dialog == GTK_WIDGET (parent) && ttw->book &&
        qof_book_is_open (ttw->book) && !qof_book_shutting_down (ttw->book) &&
        gnc_current_session_exist () && gnc_get_current_book () == ttw->book)
        table = gncTaxTableLookup (ttw->book, &request->table_guid);

    if (table && request->delete_entry)
    {
        GList *entries = gncTaxTableGetEntries (table);
        GList *node;
        GncTaxTableEntry *entry = NULL;
        for (node = entries; node; node = node->next)
            if (node->data == request->entry_identity)
            {
                entry = node->data;
                break;
            }
        if (entry && g_list_length (entries) > 1)
        {
            gnc_suspend_gui_refresh ();
            gncTaxTableBeginEdit (table);
            gncTaxTableRemoveEntry (table, entry);
            gncTaxTableEntryDestroy (entry);
            gncTaxTableChanged (table);
            gncTaxTableCommitEdit (table);
            if (ttw->current_table == table)
                ttw->current_entry = NULL;
            gnc_resume_gui_refresh ();
        }
    }
    else if (table && !request->delete_entry &&
             gncTaxTableGetRefcount (table) == 0)
    {
        gnc_suspend_gui_refresh ();
        gncTaxTableBeginEdit (table);
        gncTaxTableDestroy (table);
        gncTaxTableCommitEdit (table);
        if (ttw->current_table == table)
        {
            ttw->current_table = NULL;
            ttw->current_entry = NULL;
        }
        gnc_resume_gui_refresh ();
    }

    tax_table_window_unref (ttw);
    g_free (request);
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
        gnc_error_dialog_async (GTK_WINDOW(ttw->dialog), "%s", message);
        g_free (message);
        return;
    }

    TaxTableDeleteRequest *request = g_new0 (TaxTableDeleteRequest, 1);
    request->ttw = tax_table_window_ref (ttw);
    request->table_guid = *gncTaxTableGetGUID (ttw->current_table);
    gnc_verify_dialog_async (GTK_WINDOW (ttw->dialog), FALSE,
                             tax_table_delete_confirmed, request,
                             _("Are you sure you want to delete \"%s\"?"),
                             gncTaxTableGetName (ttw->current_table));
}

void
tax_table_new_entry_cb (GtkButton *button, TaxTableWindow *ttw)
{
    g_return_if_fail (ttw);
    if (!ttw->current_table)
        return;
    new_tax_table_dialog (ttw, FALSE, NULL, NULL, TRUE, NULL, NULL, NULL, NULL);
}

void
tax_table_edit_entry_cb (GtkButton *button, TaxTableWindow *ttw)
{
    g_return_if_fail (ttw);
    if (!ttw->current_entry)
        return;
    new_tax_table_dialog (ttw, FALSE, ttw->current_entry, NULL, TRUE,
                          NULL, NULL, NULL, NULL);
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
        gnc_error_dialog_async (GTK_WINDOW(ttw->dialog), "%s", message);
        return;
    }

    TaxTableDeleteRequest *request = g_new0 (TaxTableDeleteRequest, 1);
    request->ttw = tax_table_window_ref (ttw);
    request->table_guid = *gncTaxTableGetGUID (ttw->current_table);
    request->entry_identity = ttw->current_entry;
    request->delete_entry = TRUE;
    gnc_verify_dialog_async (GTK_WINDOW (ttw->dialog), FALSE,
                             tax_table_delete_confirmed, request,
                             "%s", _("Are you sure you want to delete this entry?"));
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
    gtk_widget_destroy (ttw->dialog);
}

void
tax_table_window_close (GtkWidget *widget, gpointer data)
{
    TaxTableWindow *ttw = data;
    gnc_close_gui_component (ttw->component_id);
}

static gboolean
tax_table_window_delete_event_cb (GtkWidget *widget,
                                  GdkEvent  *event,
                                  gpointer   user_data)
{
    TaxTableWindow *ttw = user_data;
    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP,
                          GTK_WINDOW(ttw->dialog));
    return FALSE;
}

void
tax_table_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    TaxTableWindow *ttw = data;

    if (!ttw) return;
    if (ttw->closing)
        return;
    ttw->closing = TRUE;

    gnc_unregister_gui_component (ttw->component_id);

    if (ttw->dialog)
    {
        gtk_widget_destroy (ttw->dialog);
        ttw->dialog = NULL;
    }
    tax_table_window_unref (ttw);
}

static gboolean
tax_table_window_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                               gpointer data)
{
    TaxTableWindow *ttw = data;

    if (event->keyval == GDK_KEY_Escape)
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

typedef struct
{
    gboolean destroyed;
} TaxTableOwnerStartGuard;

static void
tax_table_owner_start_destroyed ([[maybe_unused]] GtkWidget *owner,
                                 TaxTableOwnerStartGuard *guard)
{
    guard->destroyed = TRUE;
}

/* Create a tax-table window */
TaxTableWindow *
gnc_ui_tax_table_window_new (GtkWindow *parent, QofBook *book)
{
    TaxTableWindow *ttw;
    GtkBuilder *builder;
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
    if (ttw)
    {
        tax_table_window_ref (ttw);
        gtk_window_present (GTK_WINDOW(ttw->dialog));
        gboolean closing = ttw->closing;
        tax_table_window_unref (ttw);
        if (closing)
            return NULL;
        return ttw;
    }

    /* Didn't find one -- create a new window */
    ttw = g_new0 (TaxTableWindow, 1);
    ttw->ref_count = 1;
    ttw->book = book;
    ttw->session = gnc_get_current_session ();

    /* Open and read the Glade File */
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-tax-table.glade", "tax_table_window");
    ttw->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "tax_table_window"));
    g_object_set_data (G_OBJECT (ttw->dialog), "dialog_info", ttw);
    ttw->names_view = GTK_WIDGET(gtk_builder_get_object (builder, "tax_tables_view"));
    ttw->entries_view = GTK_WIDGET(gtk_builder_get_object (builder, "tax_table_entries"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(ttw->dialog), "gnc-id-new-tax-table");
    gnc_widget_style_context_add_class (GTK_WIDGET(ttw->dialog), "gnc-class-taxes");

    g_signal_connect (ttw->dialog, "delete-event",
                      G_CALLBACK(tax_table_window_delete_event_cb), ttw);

    g_signal_connect (ttw->dialog, "key_press_event",
                      G_CALLBACK (tax_table_window_key_press_cb), ttw);

    /* Create the tax tables view */
    view = GTK_TREE_VIEW(ttw->names_view);
    store = gtk_list_store_new (NUM_TAX_TABLE_COLS, G_TYPE_STRING,
                                G_TYPE_POINTER);
    gtk_tree_view_set_model (view, GTK_TREE_MODEL(store));
    g_object_unref (store);

    /* default sort order */
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(store),
                                          TAX_TABLE_COL_NAME,
                                          GTK_SORT_ASCENDING);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("", renderer,
             "text", TAX_TABLE_COL_NAME,
             NULL);
    g_object_set (G_OBJECT(column), "reorderable", TRUE, NULL);
    gtk_tree_view_append_column (view, column);
    gtk_tree_view_column_set_sort_column_id (column, TAX_TABLE_COL_NAME);

    selection = gtk_tree_view_get_selection (view);
    g_signal_connect (selection, "changed",
                      G_CALLBACK(tax_table_selection_changed), ttw);

    /* Create the tax table entries view */
    view = GTK_TREE_VIEW(ttw->entries_view);
    store = gtk_list_store_new (NUM_TAX_ENTRY_COLS, G_TYPE_STRING,
                                G_TYPE_POINTER, G_TYPE_STRING);
    gtk_tree_view_set_model (view, GTK_TREE_MODEL(store));
    g_object_unref (store);

    /* default sort order */
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(store),
                                          TAX_ENTRY_COL_NAME,
                                          GTK_SORT_ASCENDING);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("", renderer,
             "text", TAX_ENTRY_COL_NAME,
             NULL);
    g_object_set (G_OBJECT(column), "reorderable", TRUE, NULL);
    gtk_tree_view_append_column (view, column);
    gtk_tree_view_column_set_sort_column_id (column, TAX_ENTRY_COL_NAME);

    selection = gtk_tree_view_get_selection (view);
    g_signal_connect (selection, "changed",
                      G_CALLBACK(tax_table_entry_selection_changed), ttw);
    g_signal_connect (view, "row-activated",
                      G_CALLBACK(tax_table_entry_row_activated), ttw);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ttw);

    /* register with component manager */
    ttw->component_id =
        gnc_register_gui_component (DIALOG_TAX_TABLE_CM_CLASS,
                                    tax_table_window_refresh_handler,
                                    tax_table_window_close_handler,
                                    ttw);

    gnc_gui_component_set_session (ttw->component_id, ttw->session);

    tax_table_window_refresh (ttw);
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(ttw->dialog), parent);
    /* Showing a window can synchronously run user callbacks that close it.
     * Keep the controller alive until the show operation has returned. */
    tax_table_window_ref (ttw);
    gtk_widget_show_all (ttw->dialog);

    g_object_unref (G_OBJECT(builder));
    gboolean closing = ttw->closing;
    tax_table_window_unref (ttw);
    if (closing)
        return NULL;
    return ttw;
}

void
gnc_ui_tax_table_new_from_name_async (GtkWindow *parent, QofBook *book,
                                      const char *name,
                                      GncTaxTableCreateCallback callback,
                                      gpointer user_data)
{
    TaxTableWindow *ttw;
    TaxTableWindow *existing;
    GtkWindow *owner = NULL;
    TaxTableOwnerStartGuard guard = { FALSE };
    gulong owner_handler = 0;
    gboolean destroyed_during_start;

    g_return_if_fail (callback != NULL);
    if (!book || !name || !*name ||
        (parent && gtk_widget_in_destruction (GTK_WIDGET (parent))) ||
        !gnc_current_session_exist () || gnc_get_current_book () != book ||
        !qof_book_is_open (book) || qof_book_shutting_down (book) ||
        qof_book_is_readonly (book))
    {
        callback (NULL, NULL, user_data);
        return;
    }

    owner = parent ? g_object_ref (parent) : NULL;
    existing = gnc_find_first_gui_component (DIALOG_TAX_TABLE_CM_CLASS,
                                              find_handler, book);
    if (owner)
        owner_handler = g_signal_connect (owner, "destroy",
                                          G_CALLBACK (tax_table_owner_start_destroyed),
                                          &guard);
    ttw = gnc_ui_tax_table_window_new (owner, book);
    if (!ttw)
    {
        if (owner_handler && g_signal_handler_is_connected (owner, owner_handler))
            g_signal_handler_disconnect (owner, owner_handler);
        g_clear_object (&owner);
        callback (NULL, NULL, user_data);
        return;
    }

    destroyed_during_start = guard.destroyed ||
        (owner && gtk_widget_in_destruction (GTK_WIDGET (owner)));
    if (destroyed_during_start)
    {
        if (!existing && ttw->dialog && !gtk_widget_in_destruction (ttw->dialog))
            gtk_widget_destroy (ttw->dialog);
        if (owner_handler && g_signal_handler_is_connected (owner, owner_handler))
            g_signal_handler_disconnect (owner, owner_handler);
        g_clear_object (&owner);
        callback (NULL, NULL, user_data);
        return;
    }

    /* A shared tax-table window may already host an unrelated asynchronous
     * edit. Do not attach this caller to that request or replace its callback. */
    if (g_object_get_data (G_OBJECT (ttw->dialog), "tax-entry-dialog-pending"))
    {
        if (owner_handler && g_signal_handler_is_connected (owner, owner_handler))
            g_signal_handler_disconnect (owner, owner_handler);
        g_clear_object (&owner);
        callback (NULL, NULL, user_data);
        return;
    }

    gboolean started = FALSE;
    new_tax_table_dialog (ttw, TRUE, NULL, name, TRUE, parent,
                          callback, user_data, &started);
    if (!started)
    {
        GtkWidget *dialog = ttw->dialog;
        if (dialog)
            g_object_ref (dialog);
        if (dialog && !gtk_widget_in_destruction (dialog))
            gtk_widget_destroy (dialog);
        if (owner_handler && owner &&
            g_signal_handler_is_connected (owner, owner_handler))
            g_signal_handler_disconnect (owner, owner_handler);
        g_clear_object (&owner);
        callback (NULL, NULL, user_data);
        g_clear_object (&dialog);
        return;
    }
    if (owner_handler && owner &&
        g_signal_handler_is_connected (owner, owner_handler))
        g_signal_handler_disconnect (owner, owner_handler);
    g_clear_object (&owner);
}
