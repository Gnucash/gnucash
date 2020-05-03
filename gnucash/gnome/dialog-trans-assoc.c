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
#include "gnc-uri-utils.h"
#include "gnc-filepath-utils.h"
#include "Account.h"

#define DIALOG_ASSOC_CM_CLASS    "dialog-trans-assoc"
#define GNC_PREFS_GROUP          "dialogs.trans-assoc"

/** Enumeration for the tree-store */
enum GncAssocColumn {DATE_TRANS, DESC_TRANS, URI_U, AVAILABLE, URI_SPLIT, URI, URI_RELATIVE};

typedef struct
{
    GtkWidget    *window;
    GtkWidget    *view;
    const gchar  *path_head;
    gboolean      path_head_set;
}AssocDialog;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void close_handler (gpointer user_data);

static void
gnc_assoc_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_ASSOC_CM_CLASS, assoc_dialog);
    if (assoc_dialog->window)
    {
        gtk_widget_destroy (assoc_dialog->window);
        assoc_dialog->window = NULL;
    }
    g_free (assoc_dialog);
    LEAVE(" ");
}

static gboolean
gnc_assoc_dialog_window_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    if (event->keyval == GDK_KEY_Escape)
    {
        close_handler (assoc_dialog);
        return TRUE;
    }
    else
        return FALSE;
}

static gint
sort_iter_compare_func (GtkTreeModel *model,
                        GtkTreeIter  *a,
                        GtkTreeIter  *b,
                        gpointer  user_data)
{
    gint ret = 0;
    gchar *uri1, *uri2;

    gtk_tree_model_get (model, a, URI_U, &uri1, -1);
    gtk_tree_model_get (model, b, URI_U, &uri2, -1);

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

static gchar *
convert_uri_to_filename (AssocDialog *assoc_dialog, const gchar *uri, gchar *scheme)
{
    gchar *file_path = NULL;

    if (!scheme) // relative path
    {
        if (assoc_dialog->path_head_set) // not default entry
            file_path = gnc_file_path_absolute (gnc_uri_get_path (assoc_dialog->path_head), uri);
        else
            file_path = gnc_file_path_absolute (NULL, uri);
    }

    if (gnc_uri_is_file_scheme (scheme)) // absolute path
        file_path = gnc_uri_get_path (uri);

    return file_path;
}

static gchar *
convert_uri_to_unescaped (AssocDialog *assoc_dialog, const gchar *uri, gchar *scheme)
{
    gchar *uri_u = NULL;
    gchar *file_path = NULL;

    // if scheme is null or 'file' we should get a file path
    file_path = convert_uri_to_filename (assoc_dialog, uri, scheme);

    if (file_path)
    {
        uri_u = g_uri_unescape_string (file_path, NULL);
#ifdef G_OS_WIN32 // make path look like a traditional windows path
        uri_u = g_strdelimit (uri_u, "/", '\\');
#endif
    }
    else
        uri_u = g_uri_unescape_string (uri, NULL);

    g_free (file_path);

    return uri_u;
}

static void
assoc_dialog_update (AssocDialog *assoc_dialog)
{
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    gboolean          valid;

    /* disconnect the model from the treeview */
    model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));
    g_object_ref (G_OBJECT(model));
    gtk_tree_view_set_model (GTK_TREE_VIEW(assoc_dialog->view), NULL);

    /* Get first row in list store */
    valid = gtk_tree_model_get_iter_first (model, &iter);

    while (valid)
    {
        GNetworkMonitor    *nm;
        GSocketConnectable *conn;
        gchar              *uri;
        gchar              *filename;
        gchar              *scheme;

        gtk_tree_model_get (model, &iter, URI, &uri, -1);

        scheme = gnc_uri_get_scheme (uri);

        filename = convert_uri_to_unescaped (assoc_dialog, uri, scheme);

        if (!scheme || gnc_uri_is_file_scheme (scheme))
        {
            if (g_file_test (filename, G_FILE_TEST_EXISTS))
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("File Found"), -1);
            else
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("File Not Found"), -1);
        }
        else
        {
            gchar *escaped = g_uri_escape_string (uri, ":/.", TRUE);
            nm = g_network_monitor_get_default ();
            conn = g_network_address_parse_uri (escaped, 80, NULL);

            if (conn)
            {
                if (g_network_monitor_can_reach (nm, conn, NULL, NULL))
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("Address Found"), -1);
                else
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("Address Not Found"), -1);
            }
            g_free (escaped);
        }
        g_free (uri);
        g_free (scheme);
        g_free (filename);
        valid = gtk_tree_model_iter_next (model, &iter);
    }
    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW(assoc_dialog->view), model);
    g_object_unref(G_OBJECT(model));
}

static void
gnc_assoc_dialog_sort_button_cb (GtkWidget * widget, gpointer user_data)
{
    AssocDialog   *assoc_dialog = user_data;
    assoc_dialog_sort (assoc_dialog);
}

static void
gnc_assoc_dialog_check_button_cb (GtkWidget * widget, gpointer user_data)
{
    AssocDialog   *assoc_dialog = user_data;
    assoc_dialog_update (assoc_dialog);
}

static void
gnc_assoc_dialog_close_button_cb (GtkWidget * widget, gpointer user_data)
{
    AssocDialog   *assoc_dialog = user_data;
    gnc_close_gui_component_by_data (DIALOG_ASSOC_CM_CLASS, assoc_dialog);
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
        const gchar *uri_out = NULL;
        gchar *uri_out_scheme;
        gchar *uri_scheme = gnc_uri_get_scheme (uri);
        gchar *file_path = NULL;

        if (!uri_scheme) // relative path
        {
            if (assoc_dialog->path_head_set) // not default entry
                file_path = gnc_file_path_absolute (gnc_uri_get_path (assoc_dialog->path_head), uri);
            else
                file_path = gnc_file_path_absolute (NULL, uri);

            uri_out = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, file_path);
        }
        g_free (file_path);
        g_free (uri_scheme);

        if (!uri_out)
            uri_out = g_strdup (uri);

        uri_out_scheme = gnc_uri_get_scheme (uri_out);

        if (uri_out_scheme) // make sure we have a scheme entry
        {
            gnc_launch_assoc (gnc_ui_get_gtk_window(GTK_WIDGET (view)), uri_out);
            g_free (uri_out_scheme);
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
        if (!split)
            return;

        account = xaccSplitGetAccount (split);
        if (!account)
            return;

        page = gnc_plugin_page_register_new (account, FALSE);
        gnc_main_window_open_page (NULL, page);
        gsr = gnc_plugin_page_register_get_gsr (page);
        gnc_split_reg_raise (gsr);

        if (!gsr)
            return;

        gnc_split_reg_jump_to_split (gsr, split);
    }
}

static gchar*
gsr_convert_associate_uri (Transaction *trans)
{
    const gchar *uri = xaccTransGetAssociation (trans); // get the existing uri
    const gchar *part = NULL;

    if (!uri)
        return NULL;

    if (g_str_has_prefix (uri, "file:") && !g_str_has_prefix (uri,"file://"))
    {
        // fix an earlier error when storing relative paths in version 3.3
        // relative paths are stored without a leading "/" and in native form
        if (g_str_has_prefix (uri,"file:/") && !g_str_has_prefix (uri,"file://"))
            part = uri + strlen ("file:/");
        else if (g_str_has_prefix (uri,"file:") && !g_str_has_prefix (uri,"file://"))
            part = uri + strlen ("file:");

        if (part)
        {
            xaccTransSetAssociation (trans, part);
            return g_strdup (part);
        }
    }
    return g_strdup (uri);
}

static void
get_trans_info (AssocDialog *assoc_dialog)
{
    QofBook      *book = gnc_get_current_book();
    Account      *root = gnc_book_get_root_account (book);
    GList        *accts, *ptr;
    GtkTreeModel *model;
    GtkTreeIter   iter;
    GList        *splits;
    GHashTable   *trans_hash = g_hash_table_new (g_direct_hash, g_direct_equal);

    /* Get list of Accounts */
    accts = gnc_account_get_descendants_sorted (root);

    /* disconnect the model from the treeview */
    model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));
    g_object_ref (G_OBJECT(model));
    gtk_tree_view_set_model (GTK_TREE_VIEW(assoc_dialog->view), NULL);

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

            // Look for trans already in trans_hash
            if (g_hash_table_lookup (trans_hash, trans))
                continue;

            // fix an earlier error when storing relative paths in version 3.3
            uri = gsr_convert_associate_uri (trans);

            if (uri && *uri != '\0')
            {
                gchar *uri_u;
                gboolean rel = FALSE;
                gchar *scheme = gnc_uri_get_scheme (uri);
                time64 t = xaccTransRetDatePosted (trans);
                char datebuff[MAX_DATE_LENGTH + 1];
                memset (datebuff, 0, sizeof(datebuff));
                if (t == 0)
                    t = gnc_time (NULL);
                qof_print_date_buff (datebuff, MAX_DATE_LENGTH, t);
                gtk_list_store_append (GTK_LIST_STORE(model), &iter);

                if (!scheme) // path is relative
                    rel = TRUE;

                uri_u = convert_uri_to_unescaped (assoc_dialog, uri, scheme);

                gtk_list_store_set (GTK_LIST_STORE(model), &iter,
                                    DATE_TRANS, datebuff,
                                    DESC_TRANS, xaccTransGetDescription (trans),
                                    URI_U, uri_u, AVAILABLE, _("Unknown"),
                                    URI_SPLIT, split, URI, uri,
                                    URI_RELATIVE, (rel == TRUE ? "emblem-default" : NULL), -1);
                g_free (uri_u);
                g_free (scheme);
            }
            g_hash_table_insert (trans_hash, trans, trans); // add trans to trans_hash
        }
        qof_query_destroy (query);
        g_list_free (splits);
    }

    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW(assoc_dialog->view), model);
    g_object_unref(G_OBJECT(model));

    g_hash_table_destroy (trans_hash);
    g_list_free (accts);
}

static void
gnc_assoc_dialog_create (GtkWindow *parent, AssocDialog *assoc_dialog)
{
    GtkWidget         *window;
    GtkBuilder        *builder;
    GtkTreeSelection  *selection;
    GtkWidget         *path_head;
    GtkTreeViewColumn *tree_column;
    GtkCellRenderer   *cr;
    GtkWidget         *button;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-trans-assoc.glade", "list-store");
    gnc_builder_add_from_file (builder, "dialog-trans-assoc.glade", "transaction_association_window");

    window = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_association_window"));
    assoc_dialog->window = window;

    button = GTK_WIDGET(gtk_builder_get_object (builder, "sort_button"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_assoc_dialog_sort_button_cb), assoc_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "check_button"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_assoc_dialog_check_button_cb), assoc_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_assoc_dialog_close_button_cb), assoc_dialog);

    gtk_window_set_title (GTK_WINDOW(assoc_dialog->window), _("Transaction Associations"));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(window), "GncTransAssocDialog");

    assoc_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));
    path_head = GTK_WIDGET(gtk_builder_get_object (builder, "path-head"));

    assoc_dialog->path_head = gnc_prefs_get_string (GNC_PREFS_GROUP_GENERAL, "assoc-head");

    if (assoc_dialog->path_head && g_strcmp0 (assoc_dialog->path_head, "") != 0) // not default entry
    {
        gchar *path_head_ue_str = g_uri_unescape_string (assoc_dialog->path_head, NULL);
        gchar *path_head_str = gnc_uri_get_path (path_head_ue_str);
        gchar *path_head_label;
#ifdef G_OS_WIN32 // make path look like a traditional windows path
        path_head_str = g_strdelimit (path_head_str, "/", '\\');
#endif
        // test for current folder being present
        if (g_file_test (path_head_str, G_FILE_TEST_IS_DIR))
            path_head_label = g_strconcat (_("Path head for files is, "), path_head_str, NULL);
        else
            path_head_label = g_strconcat (_("Path head does not exist, "), path_head_str, NULL);

        assoc_dialog->path_head_set = TRUE;
        gtk_label_set_text (GTK_LABEL(path_head), path_head_label);
        g_free (path_head_label);
        g_free (path_head_str);
        g_free (path_head_ue_str);
    }
    else
    {
        const gchar *doc = g_get_user_special_dir (G_USER_DIRECTORY_DOCUMENTS);
        gchar *path_head_label;
        gchar *path_ret;

        if (doc)
            path_ret = g_strdup (doc);
        else
            path_ret = g_strdup (gnc_userdata_dir ());

        path_head_label = g_strdup_printf (_("Path head not set, using '%s' for relative paths"), path_ret);
        assoc_dialog->path_head_set = FALSE;
        gtk_label_set_text (GTK_LABEL(path_head), path_head_label);
        g_free (path_head_label);
        g_free (path_ret);
    }

    // Set the style context for this label so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(path_head), "gnc-class-highlight");

    /* Need to add toggle renderers here to get the xalign to work. */
    tree_column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title (tree_column, _("Relative"));
    gtk_tree_view_append_column (GTK_TREE_VIEW(assoc_dialog->view), tree_column);
    gtk_tree_view_column_set_alignment (tree_column, 0.5);
    cr = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start (tree_column, cr, TRUE);
    // connect 'active' and set 'xalign' property of the cell renderer
    gtk_tree_view_column_set_attributes (tree_column, cr, "icon-name", URI_RELATIVE, NULL);
    gtk_cell_renderer_set_alignment (cr, 0.5, 0.5);

    g_signal_connect (assoc_dialog->view, "row-activated",
                      G_CALLBACK(row_selected_cb), (gpointer)assoc_dialog);

    // set the Associate column to be the one that expands
    tree_column = GTK_TREE_VIEW_COLUMN(gtk_builder_get_object (builder, "uri-entry"));
    gtk_tree_view_column_set_expand (tree_column, TRUE);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(assoc_dialog->view), gnc_tree_view_get_grid_lines_pref ());

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(assoc_dialog->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

    g_signal_connect (assoc_dialog->window, "destroy",
                      G_CALLBACK(gnc_assoc_dialog_window_destroy_cb), assoc_dialog);

    g_signal_connect (assoc_dialog->window, "key_press_event",
                      G_CALLBACK(gnc_assoc_dialog_window_key_press_cb), assoc_dialog);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, assoc_dialog);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(assoc_dialog->window), parent);
    get_trans_info (assoc_dialog);
    gtk_widget_show_all (GTK_WIDGET(window));

    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(assoc_dialog->window));
    gtk_widget_destroy (GTK_WIDGET(assoc_dialog->window));
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
        LEAVE("No data structure");
        return(FALSE);
    }
    gtk_window_present (GTK_WINDOW(assoc_dialog->window));
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

    LEAVE(" ");
}
