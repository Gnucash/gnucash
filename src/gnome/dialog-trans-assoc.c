/********************************************************************\
 * dialog-trans-assoc.c -- Transaction associations dialog          *
 * Copyright (C) 2016 Robert Fewell                                 *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-trans-assoc.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "Query.h"
#include "Transaction.h"

#include "gnc-plugin-page-register.h"
#include "gnc-main-window.h"

#include "gnc-ui-util.h"
#include "gnc-gnome-utils.h"
#include "Account.h"

#define DIALOG_ASSOC_CM_CLASS    "dialog-trans-assoc"
#define GNC_PREFS_GROUP         "dialogs.trans-assoc"

/** Enumeration for the tree-store */
enum GncAssocColumn {DATE_TRANS, DESC_TRANS, URI, AVAILABLE, URI_SPLIT};

typedef struct
{
    GtkWidget    *dialog;
    GtkWidget    *view;
}AssocDialog;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

void gnc_assoc_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data);
void gnc_assoc_dialog_close_cb (GtkDialog *dialog, gpointer user_data);
void gnc_assoc_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer user_data);


void
gnc_assoc_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_ASSOC_CM_CLASS, assoc_dialog);

    if (assoc_dialog->dialog)
    {
        gtk_widget_destroy (assoc_dialog->dialog);
        assoc_dialog->dialog = NULL;
    }
    g_free (assoc_dialog);
    LEAVE(" ");
}

void
gnc_assoc_dialog_close_cb (GtkDialog *dialog, gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_ASSOC_CM_CLASS, assoc_dialog);
    LEAVE(" ");
}

static void
assoc_dialog_update (AssocDialog *assoc_dialog)
{
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    gboolean          valid;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));
 
    /* Get first row in list store */
    valid = gtk_tree_model_get_iter_first (model, &iter);

    while (valid)
    {
        GNetworkMonitor    *nm;
        GSocketConnectable *conn;
        gchar              *uri;
        gchar              *filename;

        gtk_tree_model_get (model, &iter, URI, &uri, -1);

        filename = g_filename_from_uri (uri, NULL, NULL);

        if (filename != NULL)
        {
            if (g_file_test (filename, G_FILE_TEST_EXISTS))
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("File Found"), -1);
            else
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("File Not Found"), -1);
        }
        else
        {
            nm = g_network_monitor_get_default ();
            conn = g_network_address_parse_uri (uri, 80, NULL);

            if (conn != NULL)
            {
                if (g_network_monitor_can_reach (nm, conn, NULL, NULL))
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("Address Found"), -1);
                else
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("Address Not Found"), -1);
            }
        }
        g_free (filename);
        g_free (uri);
        valid = gtk_tree_model_iter_next (model, &iter);
    }
}

void
gnc_assoc_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    switch (response_id)
    {
    case GTK_RESPONSE_APPLY:
        assoc_dialog_update (assoc_dialog);
        return;

    case GTK_RESPONSE_CLOSE:
    default:
        gnc_close_gui_component_by_data (DIALOG_ASSOC_CM_CLASS, assoc_dialog);
        return;
    }
}

static void
row_selected_cb (GtkTreeView *view, GtkTreePath *path,
                  GtkTreeViewColumn  *col, gpointer user_data)
{
    AssocDialog   *assoc_dialog = user_data;
    GncPluginPage *page;
    GNCSplitReg   *gsr;
    Account       *account;
    GtkTreeModel  *model;
    GtkTreeIter    iter;
    Split         *split;
    const gchar   *uri;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));

    if (!gtk_tree_model_get_iter (model, &iter, path))
      return; /* path describes a non-existing row - should not happen */

    gtk_tree_model_get (model, &iter, URI, &uri, URI_SPLIT, &split, -1);

    if ((gtk_tree_view_get_column (GTK_TREE_VIEW(assoc_dialog->view), URI) == col) ||
        (gtk_tree_view_get_column (GTK_TREE_VIEW(assoc_dialog->view), AVAILABLE) == col))
    {
        gnc_launch_assoc (uri);
    }
    else
    {
        /* This should never be true, but be paranoid */
        if (split == NULL)
            return;

        account = xaccSplitGetAccount (split);
        if (!account)
            return;

        page = gnc_plugin_page_register_new (account, FALSE);
        gnc_main_window_open_page (NULL, page);
        gsr = gnc_plugin_page_register_get_gsr (page);
        gnc_split_reg_raise (gsr);

        if (gsr == NULL)
            return;

        gnc_split_reg_jump_to_split (gsr, split);
    }
}

static void
get_trans_info (AssocDialog *assoc_dialog)
{
    QofBook      *book = gnc_get_current_book();
    Account      *root = gnc_book_get_root_account (book);
    GList        *accts, *ptr;
    GtkTreeModel *model;
    GtkTreeIter   iter;
    GList        *splits, *trans_list = NULL;

    /* Get list of Accounts */
    accts = gnc_account_get_descendants_sorted (root);

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Query  *query = qof_query_create_for (GNC_ID_SPLIT);
        Account *acc = ptr->data;

        qof_query_set_book (query, book);
        xaccQueryAddSingleAccountMatch (query, acc, QOF_QUERY_AND);

        /* Run the query */
        for (splits = qof_query_run (query); splits; splits = splits->next)
        {
            Split       *split = splits->data;
            Transaction *trans = xaccSplitGetParent (split);
            const gchar *uri;

            // Look for trans already in trans_list
            if (g_list_find (trans_list, trans) != NULL)
                continue;

            uri = xaccTransGetAssociation (trans);

            if (g_strcmp0 (uri, "") != 0 && g_strcmp0 (uri, NULL) != 0)
            {
                Timespec ts = {0,0};
                xaccTransGetDatePostedTS (trans, &ts);

                if (ts.tv_sec == 0)
                    ts.tv_sec = gnc_time (NULL);

                gtk_list_store_append (GTK_LIST_STORE(model), &iter);

                gtk_list_store_set (GTK_LIST_STORE(model), &iter,
                                    DATE_TRANS, gnc_print_date (ts),
                                    DESC_TRANS, xaccTransGetDescription (trans),
                                    URI, uri, AVAILABLE, _("Unknown"), URI_SPLIT, split, -1);
            }
            trans_list = g_list_prepend (trans_list, trans); // add trans to trans_list
        }
        qof_query_destroy (query);
        g_list_free (splits);
    }
    g_list_free (accts);
    g_list_free (trans_list);
}

static void
gnc_assoc_dialog_create (AssocDialog *assoc_dialog)
{
    GtkWidget        *dialog;
    GtkBuilder       *builder;
    GtkTreeSelection *selection;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-trans-assoc.glade", "list-store");

    gnc_builder_add_from_file (builder, "dialog-trans-assoc.glade", "Transaction Association Dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Transaction Association Dialog"));
    assoc_dialog->dialog = dialog;

    assoc_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));

    g_signal_connect (assoc_dialog->view, "row-activated",
                      G_CALLBACK(row_selected_cb), (gpointer)assoc_dialog);

    /* Enable alternative line colors */
    gtk_tree_view_set_rules_hint (GTK_TREE_VIEW(assoc_dialog->view), TRUE);

    /* default to 'close' button */
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_CLOSE);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(assoc_dialog->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, assoc_dialog);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(assoc_dialog->dialog));
    get_trans_info (assoc_dialog);

    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(assoc_dialog->dialog));
    gtk_widget_destroy (GTK_WIDGET(assoc_dialog->dialog));
    LEAVE(" ");
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    ENTER(" ");
    LEAVE(" ");
}

static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    if (!assoc_dialog)
    {
        LEAVE("No data strucure");
        return(FALSE);
    }
    gtk_window_present (GTK_WINDOW(assoc_dialog->dialog));
    LEAVE(" ");
    return(TRUE);
}

/********************************************************************\
 * gnc_trans_assoc_dialog                                           *
 * opens a window showing the Associations of all Transactions      *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_trans_assoc_dialog ()
{
    AssocDialog *assoc_dialog;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_ASSOC_CM_CLASS, show_handler, NULL))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    assoc_dialog = g_new0 (AssocDialog, 1);

    gnc_assoc_dialog_create (assoc_dialog);

    gnc_register_gui_component (DIALOG_ASSOC_CM_CLASS,
                   refresh_handler, close_handler,
                   assoc_dialog);

    gtk_widget_show (assoc_dialog->dialog);
    LEAVE(" ");
}
