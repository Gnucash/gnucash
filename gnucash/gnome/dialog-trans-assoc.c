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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-trans-assoc.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "Query.h"
#include "Transaction.h"

#include "gnc-plugin-page-register.h"
#include "gnc-main-window.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gnome-utils.h"
#include "Account.h"

#define DIALOG_ASSOC_CM_CLASS    "dialog-trans-assoc"
#define GNC_PREFS_GROUP         "dialogs.trans-assoc"

/** Enumeration for the tree-store */
enum GncAssocColumn {DATE_TRANS, DESC_TRANS, URI_U, AVAILABLE, URI_SPLIT, URI, URI_RELATIVE};

typedef struct
{
    GtkWidget    *dialog;
    GtkWidget    *view;
    const gchar  *path_head;
    gboolean      valid_path_head;
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

static gint
sort_iter_compare_func (GtkTreeModel *model,
                        GtkTreeIter  *a,
                        GtkTreeIter  *b,
                        gpointer  user_data)
{
    gint ret = 0;
    gchar *uri1, *uri2;

    gtk_tree_model_get (model, a, URI, &uri1, -1);
    gtk_tree_model_get (model, b, URI, &uri2, -1);

    ret = g_utf8_collate (uri1, uri2);

    g_free (uri1);
    g_free (uri2);

    return ret;
}

static void
assoc_dialog_sort (AssocDialog *assoc_dialog)
{
    GtkTreeModel *model;
    GtkTreeSortable *sortable;
    gint id;
    GtkSortType order;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));

    sortable = GTK_TREE_SORTABLE(model);

    if (gtk_tree_sortable_get_sort_column_id (sortable, &id, &order))
    {
        if (order == GTK_SORT_ASCENDING)
            order = GTK_SORT_DESCENDING;
        else
            order = GTK_SORT_ASCENDING;
    }
    else
    {
        gtk_tree_sortable_set_sort_func (sortable, URI, sort_iter_compare_func,
                                assoc_dialog, NULL);

        order = GTK_SORT_ASCENDING;
    }
    /* set sort order */
    gtk_tree_sortable_set_sort_column_id (sortable, URI, order);
}

static const gchar *
convert_uri_relative_to_uri (AssocDialog *assoc_dialog, const gchar *uri)
{
    const gchar *new_uri;

    if (assoc_dialog->valid_path_head && g_str_has_prefix (uri,"file:/") &&
        !g_str_has_prefix (uri,"file://")) // path is relative
    {
        const gchar *part = uri + strlen ("file:");
        new_uri = g_strconcat (assoc_dialog->path_head, part, NULL);
    }
    else
        new_uri = g_strdup (uri);

    return new_uri;
}

static gchar *
convert_uri_to_filename (AssocDialog *assoc_dialog, const gchar *uri)
{
    const gchar *new_uri = convert_uri_relative_to_uri (assoc_dialog, uri);
    gchar *filename = g_filename_from_uri (new_uri, NULL, NULL);

    return filename;
}

static gchar *
convert_uri_to_unescaped (AssocDialog *assoc_dialog, const gchar *uri)
{
    const gchar *new_uri = convert_uri_relative_to_uri (assoc_dialog, uri);
    gchar *uri_u = g_uri_unescape_string (new_uri, NULL);

    return uri_u;
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

        filename = convert_uri_to_filename (assoc_dialog, uri);

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
        g_free (uri);
        g_free (filename);
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

    case -8:
        assoc_dialog_sort (assoc_dialog);
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
    GtkTreeModel  *model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));
    GtkTreeIter    iter;
    Split         *split;
    const gchar   *uri;

    if (!gtk_tree_model_get_iter (model, &iter, path))
        return; /* path describes a non-existing row - should not happen */

    gtk_tree_model_get (model, &iter, URI, &uri, URI_SPLIT, &split, -1);

    // Open associated link
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(assoc_dialog->view), URI_U) == col)
    {
        const gchar *uri_out = convert_uri_relative_to_uri (assoc_dialog, uri);
        gchar *uri_scheme = g_uri_parse_scheme (uri_out);

        if (uri_scheme != NULL) // make sure we have a schme entry
        {
            gnc_launch_assoc (uri_out);
            g_free (uri_scheme);
        }
        else
            gnc_error_dialog (gnc_ui_get_gtk_window(GTK_WIDGET (view)),
                              "%s", _("This transaction is not associated with a valid URI."));
    }

    // Open transaction
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(assoc_dialog->view), DESC_TRANS) == col)
    {
        GncPluginPage *page;
        GNCSplitReg   *gsr;
        Account       *account;

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
                gchar *uri_u;
                gboolean rel = FALSE;
                Timespec ts = {xaccTransRetDatePosted (trans),0};

                if (ts.tv_sec == 0)
                    ts.tv_sec = gnc_time (NULL);

                gtk_list_store_append (GTK_LIST_STORE(model), &iter);

                if (g_str_has_prefix (uri,"file:/") && !g_str_has_prefix (uri,"file://")) // path is relative
                    rel = TRUE;

                uri_u = convert_uri_to_unescaped (assoc_dialog, uri);

                gtk_list_store_set (GTK_LIST_STORE(model), &iter,
                                    DATE_TRANS, gnc_print_date (ts),
                                    DESC_TRANS, xaccTransGetDescription (trans),
                                    URI_U, uri_u, AVAILABLE, _("Unknown"),
                                    URI_SPLIT, split, URI, uri,
                                    URI_RELATIVE, (rel == TRUE ? "emblem-default" : NULL), -1);
                g_free (uri_u);
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
gnc_assoc_dialog_create (GtkWindow *parent, AssocDialog *assoc_dialog)
{
    GtkWidget         *dialog;
    GtkBuilder        *builder;
    GtkTreeSelection  *selection;
    GtkWidget         *path_head;
    GtkTreeViewColumn *tree_column;
    GtkCellRenderer   *cr;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-trans-assoc.glade", "list-store");

    gnc_builder_add_from_file (builder, "dialog-trans-assoc.glade", "transaction_association_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_association_dialog"));
    assoc_dialog->dialog = dialog;

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(dialog), "GncTransAssocDialog");

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    assoc_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));
    path_head = GTK_WIDGET(gtk_builder_get_object (builder, "path-head"));

    assoc_dialog->path_head = gnc_prefs_get_string (GNC_PREFS_GROUP_GENERAL, "assoc-head");

    if ((assoc_dialog->path_head != NULL) && (g_strcmp0 (assoc_dialog->path_head, "") != 0)) // not default entry
    {
        gchar *uri_u = g_uri_unescape_string (assoc_dialog->path_head, NULL);
        gchar *path_head_str = g_filename_from_uri (uri_u, NULL, NULL);
        gchar *path_head_label;

        assoc_dialog->valid_path_head = TRUE;

        // test for current folder being present
        if (g_file_test (path_head_str, G_FILE_TEST_IS_DIR))
            path_head_label = g_strconcat (_("Path head for files is, "), path_head_str, NULL);
        else
            path_head_label = g_strconcat (_("Path head does not exist, "), path_head_str, NULL);

        gtk_label_set_text (GTK_LABEL(path_head), path_head_label);
        g_free (path_head_label);
        g_free (uri_u);
        g_free (path_head_str);
    }
    else
        assoc_dialog->valid_path_head = FALSE;

    /* Need to add toggle renderers here to get the xalign to work. */
    tree_column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title (tree_column, _("Relative"));
    gtk_tree_view_append_column (GTK_TREE_VIEW(assoc_dialog->view), tree_column);
    gtk_tree_view_column_set_alignment (tree_column, 0.5);
    gtk_tree_view_column_set_expand (tree_column, TRUE);
    cr = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start (tree_column, cr, TRUE);
    // connect 'active' and set 'xalign' property of the cell renderer
    gtk_tree_view_column_set_attributes (tree_column, cr, "icon-name", URI_RELATIVE, NULL);
    gtk_cell_renderer_set_alignment (cr, 0.5, 0.5);

    g_signal_connect (assoc_dialog->view, "row-activated",
                      G_CALLBACK(row_selected_cb), (gpointer)assoc_dialog);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(assoc_dialog->view), gnc_tree_view_get_grid_lines_pref ());

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
gnc_trans_assoc_dialog (GtkWindow *parent)
{
    AssocDialog *assoc_dialog;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_ASSOC_CM_CLASS, show_handler, NULL))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    assoc_dialog = g_new0 (AssocDialog, 1);

    gnc_assoc_dialog_create (parent, assoc_dialog);

    gnc_register_gui_component (DIALOG_ASSOC_CM_CLASS,
                   refresh_handler, close_handler,
                   assoc_dialog);

    gtk_widget_show (assoc_dialog->dialog);
    LEAVE(" ");
}
