/********************************************************************\
 * dialog-imap-editor.c -- Import Map Editor dialog                 *
 * Copyright (C) 2015 Robert Fewell                                 *
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

#include "dialog-imap-editor.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"

#include "gnc-ui-util.h"
#include "Account.h"

#define DIALOG_IMAP_CM_CLASS    "dialog-imap-edit"
#define GNC_PREFS_GROUP         "dialogs.imap-editor"

#define IMAP_FRAME_BAYES        "import-map-bayes"
#define IMAP_FRAME              "import-map"

#define IMAP_FRAME_DESC         "desc"
#define IMAP_FRAME_MEMO         "memo"
#define IMAP_FRAME_CSV          "csv-account-map"

/** Enumeration for the tree-store */
enum GncImapColumn {SOURCE_FULL_ACC, SOURCE_ACCOUNT, BASED_ON, MATCH_STRING,
                     MAP_FULL_ACC, MAP_ACCOUNT, FULL_CATEGORY, COUNT, FILTER};

typedef enum
{
    BAYES,
    NBAYES,
    ONLINE
}GncListType;

typedef struct
{
    GtkWidget    *dialog;
    QofSession   *session;
    GtkWidget    *view;
    GncListType   type;

    GtkWidget    *radio_bayes;
    GtkWidget    *radio_nbayes;
    GtkWidget    *radio_online;

    GtkWidget    *filter_button;
    GtkWidget    *filter_text_entry;
    GtkWidget    *filter_label;

    GtkWidget    *expand_button;
    GtkWidget    *collapse_button;
}ImapDialog;


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

void gnc_imap_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data);
void gnc_imap_dialog_close_cb (GtkDialog *dialog, gpointer user_data);
void gnc_imap_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer user_data);

static void get_account_info (ImapDialog *imap_dialog);

void
gnc_imap_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    ImapDialog *imap_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_IMAP_CM_CLASS, imap_dialog);

    if (imap_dialog->dialog)
    {
        gtk_widget_destroy (imap_dialog->dialog);
        imap_dialog->dialog = NULL;
    }
    g_free (imap_dialog);
    LEAVE(" ");
}

void
gnc_imap_dialog_close_cb (GtkDialog *dialog, gpointer user_data)
{
    ImapDialog *imap_dialog = user_data;

    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_IMAP_CM_CLASS, imap_dialog);
    LEAVE(" ");
}

static gboolean
are_you_sure (ImapDialog *imap_dialog)
{
    GtkWidget   *dialog;
    gint         response;
    const char  *title = _("Are you sure you want to delete the entries ?");

    dialog = gtk_message_dialog_new (GTK_WINDOW (imap_dialog->dialog),
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_QUESTION,
                                     GTK_BUTTONS_CANCEL,
                                     "%s", title);

    gtk_dialog_add_button (GTK_DIALOG(dialog), _("_Delete"), GTK_RESPONSE_ACCEPT);

    gtk_widget_grab_focus (gtk_dialog_get_widget_for_response (GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT));

    response = gtk_dialog_run (GTK_DIALOG(dialog));

    gtk_widget_destroy (dialog);

    if (response == GTK_RESPONSE_ACCEPT)
        return TRUE;
    else
        return FALSE;
}

static void
delete_info_bayes (Account *source_account, gchar *match_string, gint depth)
{
    gchar *full_category;

    full_category = g_strdup_printf (IMAP_FRAME_BAYES "/%s", match_string);
    gnc_account_delete_map_entry (source_account, full_category, TRUE);

    full_category = g_strdup_printf (IMAP_FRAME_BAYES);
    if (depth == 1) // top level, delete all below
        gnc_account_delete_map_entry (source_account, full_category, FALSE);
    else
        gnc_account_delete_map_entry (source_account, full_category, TRUE);
}

static void
delete_info_nbayes (Account *source_account, gint depth)
{
    gchar *full_category;

    full_category = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_DESC);
    gnc_account_delete_map_entry (source_account, full_category, TRUE);

    full_category = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_MEMO);
    gnc_account_delete_map_entry (source_account, full_category, TRUE);

    full_category = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_CSV);
    gnc_account_delete_map_entry (source_account, full_category, TRUE);

    full_category = g_strdup_printf (IMAP_FRAME);
    if (depth == 1) // top level, delete all below
        gnc_account_delete_map_entry (source_account, full_category, FALSE);
    else
        gnc_account_delete_map_entry (source_account, full_category, TRUE);
}

static void
delete_selected_row (GtkTreeModel *model, GtkTreeIter *iter, ImapDialog *imap_dialog)
{
    Account     *source_account = NULL;
    gchar       *full_source_account;
    gchar       *full_category;
    gchar       *match_string;

    gtk_tree_model_get (model, iter, SOURCE_ACCOUNT, &source_account, SOURCE_FULL_ACC, &full_source_account,
                                     FULL_CATEGORY, &full_category, MATCH_STRING, &match_string, -1);

    PINFO("Account is '%s', Full Category is '%s', Match String is '%s'", full_source_account, full_category, match_string);

    if (source_account != NULL)
    {
        GtkTreePath *tree_path;
        gint         depth;

        // Get the level we are at in the tree-model
        tree_path = gtk_tree_model_get_path (model, iter);
        depth = gtk_tree_path_get_depth (tree_path);
        gtk_tree_path_free (tree_path);

        gnc_account_delete_map_entry (source_account, full_category, FALSE);

        if (imap_dialog->type == BAYES)
            delete_info_bayes (source_account, match_string, depth);

        if (imap_dialog->type == NBAYES)
            delete_info_nbayes (source_account, depth);
    }
    else
        g_free (full_category);

    g_free (match_string);
    g_free (full_source_account);
}

static void
gnc_imap_dialog_delete (ImapDialog *imap_dialog)
{
    GList            *list, *row;
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    GtkTreeSelection *selection;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(imap_dialog->view));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(imap_dialog->view));

    list = gtk_tree_selection_get_selected_rows (selection, &model);

    // Make sure we have some rows selected
    if (g_list_length (list) == 0)
        return;

    // Are we sure we want to delete the entries
    if (are_you_sure (imap_dialog) == FALSE)
        return;

    // reverse list
    list = g_list_reverse (list);

    // Walk the list
    for (row = g_list_first (list); row; row = g_list_next (row))
    {
	if (gtk_tree_model_get_iter (model, &iter, row->data))
            delete_selected_row (model, &iter, imap_dialog);
    }
    g_list_foreach (list, (GFunc) gtk_tree_path_free, NULL);
    g_list_free (list);

    get_account_info (imap_dialog);
}

void
gnc_imap_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer user_data)
{
    ImapDialog *imap_dialog = user_data;

    switch (response_id)
    {
    case GTK_RESPONSE_APPLY:
        gnc_imap_dialog_delete (imap_dialog);
        return;

    case GTK_RESPONSE_CLOSE:
    default:
        gnc_close_gui_component_by_data (DIALOG_IMAP_CM_CLASS, imap_dialog);
        return;
    }
}

static gboolean
filter_test_and_move_next (GtkTreeModel *model, GtkTreeIter *iter, ImapDialog *imap_dialog)
{
    GtkTreePath *tree_path;
    gint         depth;
    gboolean     valid;
    const gchar *filter_text;
    const gchar *match_string;
    const gchar *map_full_acc;

    // Read the row
    gtk_tree_model_get (model, iter, MATCH_STRING, &match_string, MAP_FULL_ACC, &map_full_acc, -1);

    filter_text = gtk_entry_get_text (GTK_ENTRY(imap_dialog->filter_text_entry));

    // Get the level we are at in the tree-model
    tree_path = gtk_tree_model_get_path (model, iter);
    depth = gtk_tree_path_get_depth (tree_path);

    // Reset filter to TRUE
    gtk_tree_store_set (GTK_TREE_STORE(model), iter, FILTER, TRUE, -1);

    // Check for a filter_text entry
    if (g_strcmp0 (filter_text, "") != 0)
    {
        if (match_string != NULL) // Check for match_string is not NULL, valid line
        {
            if ((g_strrstr (match_string, filter_text) == NULL) &&
                (g_strrstr (map_full_acc, filter_text) == NULL ))
                gtk_tree_store_set (GTK_TREE_STORE(model), iter, FILTER, FALSE, -1);
            else
                gtk_tree_view_expand_to_path (GTK_TREE_VIEW(imap_dialog->view), tree_path);
        }
    }
    // Select next entry based on path
    if (depth == 1)
        gtk_tree_path_down (tree_path);
    else
    {
        gtk_tree_path_next (tree_path);
        if (!gtk_tree_model_get_iter (model, iter, tree_path))
        {
            gtk_tree_path_prev (tree_path);
            gtk_tree_path_up (tree_path);
            gtk_tree_path_next (tree_path);
        }
    }
    valid = gtk_tree_model_get_iter (model, iter, tree_path);

    gtk_tree_path_free (tree_path);

    return valid;
}

static void
filter_button_cb (GtkButton *button, ImapDialog *imap_dialog)
{
    GtkTreeModel *model, *filter;
    GtkTreeIter   iter;
    gboolean      valid;

    filter = gtk_tree_view_get_model (GTK_TREE_VIEW(imap_dialog->view));
    model = gtk_tree_model_filter_get_model (GTK_TREE_MODEL_FILTER(filter));

    // Collapse all nodes
    gtk_tree_view_collapse_all (GTK_TREE_VIEW(imap_dialog->view));

    valid = gtk_tree_model_get_iter_first (model, &iter);

    while (valid)
    {
        valid = filter_test_and_move_next (model, &iter, imap_dialog);
    }
}

static void
expand_button_cb (GtkButton *button, ImapDialog *imap_dialog)
{
    // Clear the filter
    gtk_entry_set_text (GTK_ENTRY(imap_dialog->filter_text_entry), "");

    filter_button_cb (button, imap_dialog);

    gtk_tree_view_expand_all (GTK_TREE_VIEW(imap_dialog->view));
}

static void
collapse_button_cb (GtkButton *button, ImapDialog *imap_dialog)
{
    // Clear the filter
    gtk_entry_set_text (GTK_ENTRY(imap_dialog->filter_text_entry), "");

    filter_button_cb (button, imap_dialog);

    gtk_tree_view_collapse_all (GTK_TREE_VIEW(imap_dialog->view));
}

static void
list_type_selected_cb (GtkToggleButton* button, ImapDialog *imap_dialog)
{
    GncListType type;

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(imap_dialog->radio_bayes)))
        type = BAYES;
    else if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(imap_dialog->radio_nbayes)))
        type = NBAYES;
    else
        type = ONLINE;

    // Lets do this only on change of list type
    if (type != imap_dialog->type)
    {
        imap_dialog->type = type;
        get_account_info (imap_dialog);
    }
}

static void
show_count_column (ImapDialog *imap_dialog, gboolean show)
{
    GtkTreeViewColumn *tree_column;

    // Show Count Column
    tree_column = gtk_tree_view_get_column (GTK_TREE_VIEW(imap_dialog->view), 4);
    gtk_tree_view_column_set_visible (tree_column, show);

    // Hide Based on Column
    tree_column = gtk_tree_view_get_column (GTK_TREE_VIEW(imap_dialog->view), 1);
    gtk_tree_view_column_set_visible (tree_column, !show);

    gtk_tree_view_columns_autosize (GTK_TREE_VIEW(imap_dialog->view));
}

static void
add_to_store (GtkTreeModel *model, GtkTreeIter *iter, const gchar *text, GncImapInfo *imapInfo)
{
    gchar       *fullname = NULL;
    gchar       *map_fullname = NULL;

    fullname = gnc_account_get_full_name (imapInfo->source_account);

    // Do we have a valid map account
    if (imapInfo->map_account == NULL)
        map_fullname = g_strdup (_("Map Account NOT found"));
    else
        map_fullname = gnc_account_get_full_name (imapInfo->map_account);

    PINFO("Add to Store: Source Acc '%s', Match '%s', Map Acc '%s'", fullname, imapInfo->match_string, map_fullname);

    gtk_tree_store_set (GTK_TREE_STORE(model), iter,
                        SOURCE_FULL_ACC, fullname, SOURCE_ACCOUNT, imapInfo->source_account,
                        BASED_ON, text,
                        MATCH_STRING, imapInfo->match_string,
                        MAP_FULL_ACC, map_fullname, MAP_ACCOUNT, imapInfo->map_account,
                        FULL_CATEGORY, imapInfo->full_category, COUNT, imapInfo->count,
                        FILTER, TRUE, -1);

    g_free (fullname);
    g_free (map_fullname);
}

static void
get_imap_info (Account *acc, const gchar *category, GtkTreeModel *model, const gchar *text)
{
    GtkTreeIter  toplevel, child;
    GList *imap_list, *node;
    gchar *acc_name = NULL;

    acc_name = gnc_account_get_full_name (acc);
    PINFO("Source Acc '%s', Based on '%s', Path Head '%s'", acc_name, text, category);

    if (category == NULL) // For Bayesian, category is NULL
        imap_list = gnc_account_imap_get_info_bayes (acc);
    else
        imap_list = gnc_account_imap_get_info (acc, category);

    if (g_list_length (imap_list) > 0)
    {
        PINFO("List length is %d", g_list_length (imap_list));

        // Add top level entry of Source full Account and Based on.
        gtk_tree_store_append (GTK_TREE_STORE(model), &toplevel, NULL);
        gtk_tree_store_set (GTK_TREE_STORE(model), &toplevel,
                        SOURCE_ACCOUNT, acc, SOURCE_FULL_ACC, acc_name, BASED_ON, text, FILTER, TRUE, -1);

        for (node = imap_list;  node; node = g_list_next (node))
        {
            GncImapInfo *imapInfo = node->data;

            // First add a child entry and pass iter to add_to_store
            gtk_tree_store_append (GTK_TREE_STORE(model), &child, &toplevel);
            add_to_store (model, &child, text, imapInfo);

            // Free the members and structure
            g_free (imapInfo->category_head);
            g_free (imapInfo->full_category);
            g_free (imapInfo->match_string);
            g_free (imapInfo->count);
            g_free (imapInfo);
        }
    }
    g_free (acc_name);
    g_list_free (imap_list); // Free the List
}

static void
show_first_row (ImapDialog *imap_dialog)
{
    GtkTreeModel *model, *filter;
    GtkTreeIter   iter;

    filter = gtk_tree_view_get_model (GTK_TREE_VIEW(imap_dialog->view));
    model = gtk_tree_model_filter_get_model (GTK_TREE_MODEL_FILTER(filter));

    // See if there are any entries
    if (gtk_tree_model_get_iter_first (model, &iter))
    {
        GtkTreePath *path;
        path = gtk_tree_path_new_first (); // Set Path to first entry
        gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(imap_dialog->view), path, NULL, TRUE, 0.0, 0.0);
        gtk_tree_path_free (path);
    }
}

static void
get_account_info_bayes (GList *accts, GtkTreeModel *model)
{
    GList   *ptr;

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = ptr->data;

        get_imap_info (acc, NULL, model, _("Bayesian"));
    }
}

static void
get_account_info_nbayes (GList *accts, GtkTreeModel *model)
{
    GList   *ptr;

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = ptr->data;

        // Description
        get_imap_info (acc, IMAP_FRAME_DESC, model, _("Description Field"));

        // Memo
        get_imap_info (acc, IMAP_FRAME_MEMO, model, _("Memo Field"));

        // CSV Account Map
        get_imap_info (acc, IMAP_FRAME_CSV, model, _("CSV Account Map"));
    }
}

static void
get_account_info_online (GList *accts, GtkTreeModel *model)
{
    GList       *ptr;
    GtkTreeIter  toplevel;

    GncImapInfo imapInfo;

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        gchar  *text = NULL;
        Account *acc = ptr->data;

        // Save source account
        imapInfo.source_account = acc;

        imapInfo.full_category = "online_id";

        text = gnc_account_get_map_entry (acc, imapInfo.full_category);

        if (text != NULL)
        {
            if (g_strcmp0 (text, "") == 0)
                imapInfo.map_account = NULL;
            else
                imapInfo.map_account = imapInfo.source_account;

            imapInfo.match_string  = text;
            imapInfo.count = " ";

            // Add top level entry and pass iter to add_to_store
            gtk_tree_store_append (GTK_TREE_STORE(model), &toplevel, NULL);
            add_to_store (model, &toplevel, _("Online Id"), &imapInfo);
        }
        g_free (text);
    }
}

static void
show_filter_option (ImapDialog *imap_dialog, gboolean show)
{
    if (show)
    {
        gtk_widget_show (imap_dialog->filter_text_entry);
        gtk_widget_show (imap_dialog->filter_button);
        gtk_widget_show (imap_dialog->filter_label);
        gtk_widget_show (imap_dialog->expand_button);
        gtk_widget_show (imap_dialog->collapse_button);
    }
    else
    {
        gtk_widget_hide (imap_dialog->filter_text_entry);
        gtk_widget_hide (imap_dialog->filter_button);
        gtk_widget_hide (imap_dialog->filter_label);
        gtk_widget_hide (imap_dialog->expand_button);
        gtk_widget_hide (imap_dialog->collapse_button);
    }
}

static void
get_account_info (ImapDialog *imap_dialog)
{
    Account      *root;
    GList        *accts;
    GtkTreeModel *model, *filter;

    /* Get list of Accounts */
    root = gnc_book_get_root_account (gnc_get_current_book());
    accts = gnc_account_get_descendants_sorted (root);

    filter = gtk_tree_view_get_model (GTK_TREE_VIEW(imap_dialog->view));

    model = gtk_tree_model_filter_get_model (GTK_TREE_MODEL_FILTER(filter));

    // Clear the tree store
    gtk_tree_store_clear (GTK_TREE_STORE(model));

    // Clear the filter
    gtk_entry_set_text (GTK_ENTRY(imap_dialog->filter_text_entry), "");

    // Hide Count Column
    show_count_column (imap_dialog, FALSE);

    // Show Filter Option
    show_filter_option (imap_dialog, TRUE);

    if (imap_dialog->type == BAYES)
    {
        get_account_info_bayes (accts, model);

        // Show Count Column
        show_count_column (imap_dialog, TRUE);
    }
    else if (imap_dialog->type == NBAYES)
        get_account_info_nbayes (accts, model);
    else if (imap_dialog->type == ONLINE)
    {
        // Hide Filter Option
        show_filter_option (imap_dialog, FALSE);
        get_account_info_online (accts, model);
    }
    // if there are any entries, show first row
    show_first_row (imap_dialog);

    g_list_free (accts);
}

static void
gnc_imap_dialog_create (GtkWidget *parent, ImapDialog *imap_dialog)
{
    GtkWidget        *dialog;
    GtkBuilder       *builder;
    GtkTreeModel     *filter;
    GtkTreeSelection *selection;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-imap-editor.glade", "tree-store");
    gnc_builder_add_from_file (builder, "dialog-imap-editor.glade", "treemodelfilter");
    gnc_builder_add_from_file (builder, "dialog-imap-editor.glade", "import_map_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "import_map_dialog"));
    imap_dialog->dialog = dialog;

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(dialog), "GncImapDialog");

    imap_dialog->session = gnc_get_current_session();
    imap_dialog->type = BAYES;

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    /* Connect the radio buttons...*/
    imap_dialog->radio_bayes = GTK_WIDGET(gtk_builder_get_object (builder, "radio-bayes"));
    imap_dialog->radio_nbayes = GTK_WIDGET(gtk_builder_get_object (builder, "radio-nbayes"));
    imap_dialog->radio_online = GTK_WIDGET(gtk_builder_get_object (builder, "radio-online"));
    g_signal_connect (imap_dialog->radio_bayes, "toggled",
                      G_CALLBACK(list_type_selected_cb), (gpointer)imap_dialog);
    g_signal_connect (imap_dialog->radio_nbayes, "toggled",
                      G_CALLBACK(list_type_selected_cb), (gpointer)imap_dialog);

    imap_dialog->filter_text_entry = GTK_WIDGET(gtk_builder_get_object (builder, "filter-text-entry"));
    imap_dialog->filter_label = GTK_WIDGET(gtk_builder_get_object (builder, "filter-label"));
    imap_dialog->filter_button = GTK_WIDGET(gtk_builder_get_object (builder, "filter-button"));
    g_signal_connect (imap_dialog->filter_button, "clicked",
                      G_CALLBACK(filter_button_cb), (gpointer)imap_dialog);

    imap_dialog->expand_button = GTK_WIDGET(gtk_builder_get_object (builder, "expand-button"));
    g_signal_connect (imap_dialog->expand_button, "clicked",
                      G_CALLBACK(expand_button_cb), (gpointer)imap_dialog);

    imap_dialog->collapse_button = GTK_WIDGET(gtk_builder_get_object (builder, "collapse-button"));
    g_signal_connect (imap_dialog->collapse_button, "clicked",
                      G_CALLBACK(collapse_button_cb), (gpointer)imap_dialog);

    imap_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));

    // Set filter column
    filter = gtk_tree_view_get_model (GTK_TREE_VIEW(imap_dialog->view));
    gtk_tree_model_filter_set_visible_column (GTK_TREE_MODEL_FILTER(filter), FILTER);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(imap_dialog->view), gnc_tree_view_get_grid_lines_pref ());

    /* default to 'close' button */
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_CLOSE);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(imap_dialog->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, imap_dialog);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(imap_dialog->dialog));
    get_account_info (imap_dialog);

    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    ImapDialog *imap_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(imap_dialog->dialog));
    gtk_widget_destroy (GTK_WIDGET(imap_dialog->dialog));
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
    ImapDialog *imap_dialog = user_data;

    ENTER(" ");
    if (!imap_dialog)
    {
        LEAVE("No data strucure");
        return(FALSE);
    }
    gtk_window_present (GTK_WINDOW(imap_dialog->dialog));
    LEAVE(" ");
    return(TRUE);
}

/********************************************************************\
 * gnc_imap_dialog                                                  *
 * opens a window showing Bayesian and Non-Bayesian information     *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_imap_dialog (GtkWidget *parent)
{
    ImapDialog *imap_dialog;
    gint component_id;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_IMAP_CM_CLASS, show_handler, NULL))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    imap_dialog = g_new0 (ImapDialog, 1);

    gnc_imap_dialog_create (parent, imap_dialog);

    component_id = gnc_register_gui_component (DIALOG_IMAP_CM_CLASS,
                   refresh_handler, close_handler,
                   imap_dialog);

    gnc_gui_component_set_session (component_id, imap_dialog->session);

    gtk_widget_show (imap_dialog->dialog);
    LEAVE(" ");
}
