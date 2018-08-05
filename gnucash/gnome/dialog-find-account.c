/********************************************************************\
 * dialog-find-account.c -- Find Account dialog                     *
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

#include "dialog-find-account.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"

#include "gnc-ui-util.h"
#include "Account.h"
#include "gnc-plugin-page-account-tree.h"
#include "dialog-account.h"

#define DIALOG_FIND_ACCOUNT_CM_CLASS    "dialog-find-account"
#define GNC_PREFS_GROUP                 "dialogs.find-account"

/** Enumeration for the tree-store */
enum GncFindAccountColumn {ACC_FULL_NAME, ACCOUNT, PLACE_HOLDER, HIDDEN, NOT_USED, BAL_ZERO};

typedef struct
{
    GtkWidget    *dialog;
    GtkWidget    *parent;
    QofSession   *session;
    Account      *account;
    GtkWidget    *view;

    GtkWidget    *radio_hbox;
    GtkWidget    *radio_root;
    GtkWidget    *radio_subroot;

    GtkWidget    *filter_button;
    GtkWidget    *filter_text_entry;
    GtkWidget    *sub_label;

    gboolean      jump_close;

}FindAccountDialog;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

void gnc_find_account_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data);
void gnc_find_account_dialog_close_cb (GtkDialog *dialog, gpointer user_data);
void gnc_find_account_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer user_data);

void
gnc_find_account_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_FIND_ACCOUNT_CM_CLASS, facc_dialog);

    if (facc_dialog->dialog)
    {
        gtk_widget_destroy (facc_dialog->dialog);
        facc_dialog->dialog = NULL;
    }
    g_free (facc_dialog);
    LEAVE(" ");
}

void
gnc_find_account_dialog_close_cb (GtkDialog *dialog, gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;

    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_FIND_ACCOUNT_CM_CLASS, facc_dialog);
    LEAVE(" ");
}

static void
jump_to_account (FindAccountDialog *facc_dialog, Account *jump_account)
{
    if (jump_account != NULL)
        gnc_plugin_page_account_tree_open (jump_account, GTK_WINDOW(facc_dialog->parent));

    if (facc_dialog->jump_close == TRUE)
        gnc_find_account_dialog_close_cb (GTK_DIALOG(facc_dialog->dialog), facc_dialog);
}

static void
gnc_find_account_dialog_jump_set (FindAccountDialog *facc_dialog)
{
    if (facc_dialog->jump_close == TRUE)
        facc_dialog->jump_close = FALSE;
    else
        facc_dialog->jump_close = TRUE;
}

static void
gnc_find_account_dialog_jump_to (FindAccountDialog *facc_dialog)
{
    Account          *jump_account = NULL;
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    GtkTreeSelection *selection;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(facc_dialog->view));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(facc_dialog->view));

    if (gtk_tree_selection_get_selected (selection, &model, &iter))
        gtk_tree_model_get (model, &iter, ACCOUNT, &jump_account,  -1);

    jump_to_account (facc_dialog, jump_account);
}

static void
row_double_clicked (GtkTreeView *treeview, GtkTreePath *path,
                    GtkTreeViewColumn *col, FindAccountDialog *facc_dialog)
{
    Account      *jump_account = NULL;
    GtkTreeModel *model;
    GtkTreeIter   iter;

    model = gtk_tree_view_get_model (treeview);

    if (gtk_tree_model_get_iter (model, &iter, path))
       gtk_tree_model_get (model, &iter, ACCOUNT, &jump_account, -1);

    jump_to_account (facc_dialog, jump_account);
}

void
gnc_find_account_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;

    switch (response_id)
    {
    case GTK_RESPONSE_APPLY:
        gnc_find_account_dialog_jump_to (facc_dialog);
        return;

    case GTK_RESPONSE_YES:
        gnc_find_account_dialog_jump_set (facc_dialog);
        return;

    case GTK_RESPONSE_CLOSE:
    default:
        gnc_close_gui_component_by_data (DIALOG_FIND_ACCOUNT_CM_CLASS, facc_dialog);
        return;
    }
}

static void
fill_model (FindAccountDialog *facc_dialog, Account *account)
{
    GtkTreeModel *model;
    GtkTreeIter   iter;
    gchar        *fullname = gnc_account_get_full_name (account);
    gint          splits = xaccAccountCountSplits (account, TRUE);
    gnc_numeric   total = xaccAccountGetBalanceInCurrency (account, NULL, TRUE);

    PINFO("Add to Store: Account '%s'", fullname);

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(facc_dialog->view));

    gtk_list_store_append (GTK_LIST_STORE(model), &iter);

    gtk_list_store_set (GTK_LIST_STORE(model), &iter,
                        ACC_FULL_NAME, fullname, ACCOUNT, account,
                        PLACE_HOLDER, (xaccAccountGetPlaceholder (account) == TRUE ? "emblem-default" : NULL),
                        HIDDEN, (xaccAccountGetHidden (account) == TRUE ? "emblem-default" : NULL),
                        NOT_USED, (splits == 0 ? "emblem-default" : NULL),
                        BAL_ZERO, (gnc_numeric_zero_p (total) == TRUE ? "emblem-default" : NULL), -1);
    g_free (fullname);
}

static void
get_account_info (FindAccountDialog *facc_dialog)
{
    Account      *root;
    GList        *accts;
    GList        *ptr;
    gchar        *filter_text;
    gboolean      radio_root;

    /* Get the state of the root radio button */
    radio_root = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(facc_dialog->radio_root));

     /* Get list of Accounts */
    if ((facc_dialog->account == NULL) || (radio_root == TRUE))
        root = gnc_book_get_root_account (gnc_get_current_book());
    else
        root = facc_dialog->account;

    accts = gnc_account_get_descendants_sorted (root);

    filter_text = g_ascii_strdown (gtk_entry_get_text (GTK_ENTRY(facc_dialog->filter_text_entry)), -1);

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = ptr->data;
        gchar   *full_name = gnc_account_get_full_name (acc);
        gchar   *match_string = g_ascii_strdown (full_name, -1);

        if ((g_strcmp0 (filter_text, "") == 0) || (g_strrstr (match_string, filter_text) != NULL))
            fill_model (facc_dialog, acc);

        g_free (match_string);
        g_free (full_name);
    }
    g_free (filter_text);
    g_list_free (accts);

    gtk_tree_view_columns_autosize (GTK_TREE_VIEW(facc_dialog->view));
}

static void
filter_button_cb (GtkButton *button, FindAccountDialog *facc_dialog)
{
    GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW(facc_dialog->view));

    // Clear the list store
    gtk_list_store_clear (GTK_LIST_STORE(model));

    get_account_info (facc_dialog);

    // Clear the filter
    gtk_entry_set_text (GTK_ENTRY(facc_dialog->filter_text_entry), "");
}

static void
gnc_find_account_dialog_create (GtkWidget *parent, FindAccountDialog *facc_dialog)
{
    GtkWidget         *dialog;
    GtkBuilder        *builder;
    GtkTreeSelection  *selection;

    GtkTreeViewColumn *tree_column;
    GtkCellRenderer   *cr;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-find-account.glade", "list-store");
    gnc_builder_add_from_file (builder, "dialog-find-account.glade", "find_account_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "find_account_dialog"));
    facc_dialog->dialog = dialog;

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(dialog), "GncFindAccountDialog");

    facc_dialog->session = gnc_get_current_session();

    /* parent */
    if (parent != NULL)
    {
        facc_dialog->parent = parent;
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));
    }
    else
        facc_dialog->parent = NULL;

    /* Connect the radio buttons...*/
    facc_dialog->radio_root = GTK_WIDGET(gtk_builder_get_object (builder, "radio-root"));
    facc_dialog->radio_subroot = GTK_WIDGET(gtk_builder_get_object (builder, "radio-subroot"));

    facc_dialog->filter_text_entry = GTK_WIDGET(gtk_builder_get_object (builder, "filter-text-entry"));
    facc_dialog->sub_label = GTK_WIDGET(gtk_builder_get_object (builder, "sub-label"));
    facc_dialog->radio_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "hbox-radio"));
    facc_dialog->filter_button = GTK_WIDGET(gtk_builder_get_object (builder, "filter-button"));
    g_signal_connect (facc_dialog->filter_button, "clicked",
                      G_CALLBACK(filter_button_cb), (gpointer)facc_dialog);

    facc_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));
    g_signal_connect (facc_dialog->view, "row-activated",
                     G_CALLBACK(row_double_clicked), (gpointer)facc_dialog);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(facc_dialog->view), gnc_tree_view_get_grid_lines_pref ());

    /* default to 'close' button */
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_CLOSE);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(facc_dialog->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

    /* Need to add pixbuf renderers here to get the xalign to work. */
    tree_column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title (tree_column, _("Place Holder"));
    gtk_tree_view_append_column (GTK_TREE_VIEW(facc_dialog->view), tree_column);
    gtk_tree_view_column_set_alignment (tree_column, 0.5);
    gtk_tree_view_column_set_expand (tree_column, TRUE);
    cr = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start (tree_column, cr, TRUE);
    // connect 'active' and set 'xalign' property of the cell renderer
    gtk_tree_view_column_set_attributes (tree_column, cr, "icon-name", PLACE_HOLDER, NULL);
    gtk_cell_renderer_set_alignment (cr, 0.5, 0.5);

    tree_column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title (tree_column, _("Hidden"));
    gtk_tree_view_append_column (GTK_TREE_VIEW(facc_dialog->view), tree_column);
    gtk_tree_view_column_set_alignment (tree_column, 0.5);
    gtk_tree_view_column_set_expand (tree_column, TRUE);
    cr = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start (tree_column, cr, TRUE);
    // connect 'active' and set 'xalign' property of the cell renderer
    gtk_tree_view_column_set_attributes (tree_column, cr, "icon-name", HIDDEN, NULL);
    gtk_cell_renderer_set_alignment (cr, 0.5, 0.5);

    tree_column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title (tree_column, _("Not Used"));
    gtk_tree_view_append_column (GTK_TREE_VIEW(facc_dialog->view), tree_column);
    gtk_tree_view_column_set_alignment (tree_column, 0.5);
    gtk_tree_view_column_set_expand (tree_column, TRUE);
    cr = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start (tree_column, cr, TRUE);
    // connect 'active' and set 'xalign' property of the cell renderer
    gtk_tree_view_column_set_attributes (tree_column, cr, "icon-name", NOT_USED, NULL);
    gtk_cell_renderer_set_alignment (cr, 0.5, 0.5);

    tree_column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title (tree_column, _("Balance Zero"));
    gtk_tree_view_append_column (GTK_TREE_VIEW(facc_dialog->view), tree_column);
    gtk_tree_view_column_set_alignment (tree_column, 0.5);
    gtk_tree_view_column_set_expand (tree_column, TRUE);
    cr = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start (tree_column, cr, TRUE);
    // connect 'active' and set 'xalign' property of the cell renderer
    gtk_tree_view_column_set_attributes (tree_column, cr, "icon-name", BAL_ZERO, NULL);
    gtk_cell_renderer_set_alignment (cr, 0.5, 0.5);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, facc_dialog);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(facc_dialog->dialog));

    if (facc_dialog->account != NULL)
    {
        const gchar *sub_label_start = _("Search from ");
        gchar *sub_full_name = gnc_account_get_full_name (facc_dialog->account);
        gchar *sub_label;

        sub_label = g_strconcat (sub_label_start, sub_full_name, NULL);
        gtk_button_set_label (GTK_BUTTON(facc_dialog->radio_subroot), sub_label);
        g_free (sub_full_name);
        g_free (sub_label);

        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(facc_dialog->radio_subroot), TRUE);
        gtk_widget_show_all (facc_dialog->radio_hbox);
    }
    else
        gtk_widget_hide (facc_dialog->radio_hbox);

    // Set the filter to Wildcard
    gtk_entry_set_text (GTK_ENTRY(facc_dialog->filter_text_entry), "");

    get_account_info (facc_dialog);

    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(facc_dialog->dialog));
    gtk_widget_destroy (GTK_WIDGET(facc_dialog->dialog));
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
    FindAccountDialog *facc_dialog = user_data;

    ENTER(" ");
    if (!facc_dialog)
    {
        LEAVE("No data strucure");
        return(FALSE);
    }
    gtk_window_present (GTK_WINDOW(facc_dialog->dialog));
    LEAVE(" ");
    return(TRUE);
}

/********************************************************************\
 * gnc_find_account_dialog                                          *
 * opens a window allowing for searches on account names            *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_find_account_dialog (GtkWidget *parent, Account *account)
{
    FindAccountDialog *facc_dialog;
    gint component_id;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_FIND_ACCOUNT_CM_CLASS, show_handler, NULL))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    facc_dialog = g_new0 (FindAccountDialog, 1);

    facc_dialog->account = account;
    facc_dialog->jump_close = TRUE;

    gnc_find_account_dialog_create (parent, facc_dialog);

    component_id = gnc_register_gui_component (DIALOG_FIND_ACCOUNT_CM_CLASS,
                   refresh_handler, close_handler,
                   facc_dialog);

    gnc_gui_component_set_session (component_id, facc_dialog->session);

    gtk_widget_show (facc_dialog->dialog);
    LEAVE(" ");
}
