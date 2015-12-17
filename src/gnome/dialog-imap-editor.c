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

#include "config.h"

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

/** Enumeration for the liststore */
enum GncImapColumn {SOURCE_FULL_ACC, SOURCE_ACCOUNT, BASED_ON, MATCH_STRING,
                     MAP_FULL_ACC, MAP_ACCOUNT, KVP_PATH, PROBABILITY};

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
    GtkWidget    *store;
    GtkWidget    *view;
    GncListType   type;

    GtkWidget    *radio_bayes;
    GtkWidget    *radio_nbayes;
    GtkWidget    *radio_online;

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

    for (row = g_list_first (list); row; row = g_list_next (row))
    {
	if (gtk_tree_model_get_iter (model, &iter, row->data))
        {
            Account *source_account = NULL;
            gchar   *full_source_account;
            gchar   *kvp_path;
            gchar   *match_string;

            gtk_tree_model_get (model, &iter, SOURCE_ACCOUNT, &source_account, SOURCE_FULL_ACC, &full_source_account,
                                              KVP_PATH, &kvp_path, MATCH_STRING, &match_string, -1);

            PINFO("Account is '%s', Path is '%s', Search is '%s'", full_source_account, kvp_path, match_string);

            if (source_account != NULL)
            {
                gnc_account_delete_kvp (source_account, kvp_path, FALSE);

                if (imap_dialog->type == BAYES)
                {
                    kvp_path = g_strdup_printf (IMAP_FRAME_BAYES "/%s", match_string);
                    gnc_account_delete_kvp (source_account, kvp_path, TRUE);

                    kvp_path = g_strdup_printf (IMAP_FRAME_BAYES);
                    gnc_account_delete_kvp (source_account, kvp_path, TRUE);
                }

                if (imap_dialog->type == NBAYES)
                {
                    kvp_path = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_DESC);
                    gnc_account_delete_kvp (source_account, kvp_path, TRUE);

                    kvp_path = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_MEMO);
                    gnc_account_delete_kvp (source_account, kvp_path, TRUE);

                    kvp_path = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_CSV);
                    gnc_account_delete_kvp (source_account, kvp_path, TRUE);

                    kvp_path = g_strdup_printf (IMAP_FRAME);
                    gnc_account_delete_kvp (source_account, kvp_path, TRUE);
                }
            }
            g_free (match_string);
            g_free (full_source_account);
	}
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

static void
list_type_selected (GtkToggleButton* button, ImapDialog *imap_dialog)
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
show_probability_column (ImapDialog *imap_dialog, gboolean show)
{
    GtkTreeViewColumn *tree_column;

    // Show Probability Column
    tree_column = gtk_tree_view_get_column (GTK_TREE_VIEW(imap_dialog->view), 4);
    gtk_tree_view_column_set_visible (tree_column, show);

    // Hide Based on Column
    tree_column = gtk_tree_view_get_column (GTK_TREE_VIEW(imap_dialog->view), 1);
    gtk_tree_view_column_set_visible (tree_column, !show);
}

static void
add_to_store (GtkTreeModel *store, const gchar *text, gpointer user_data)
{
    GtkTreeIter  iter;
    gchar       *fullname = NULL;
    gchar       *map_fullname = NULL;

    struct kvp_info *kvpInfo = (struct kvp_info*)user_data;

    gtk_list_store_append (GTK_LIST_STORE(store), &iter);

    fullname = gnc_account_get_full_name (kvpInfo->source_account);

    map_fullname = gnc_account_get_full_name (kvpInfo->map_account);

    PINFO("Add to Store: Source Acc '%s', Match '%s', Map Acc '%s'", fullname, kvpInfo->match_string, map_fullname);

    gtk_list_store_set (GTK_LIST_STORE(store), &iter,
                        SOURCE_FULL_ACC, fullname, SOURCE_ACCOUNT, kvpInfo->source_account,
                        BASED_ON, text,
                        MATCH_STRING, kvpInfo->match_string,
                        MAP_FULL_ACC, map_fullname, MAP_ACCOUNT, kvpInfo->map_account,
                        KVP_PATH, kvpInfo->kvp_path, PROBABILITY, kvpInfo->probability, -1);

    g_free (fullname);
    g_free (map_fullname);
}

static void
get_imap_info (Account *acc, const gchar *category, GtkTreeModel *store, const gchar *text)
{
    GList *kvp_list, *node;
    gchar *acc_name = NULL;

    acc_name = gnc_account_get_full_name (acc);
    PINFO("Source Acc '%s', Based on '%s', Path Head '%s'", acc_name, text, category);
    g_free (acc_name);

    if (category == NULL) // For Bayesian, category is NULL
        kvp_list = gnc_account_imap_get_info_bayes (acc, category);
    else
        kvp_list = gnc_account_imap_get_info (acc, category);

    if (g_list_length (kvp_list) > 0)
    {
        PINFO("List length is %d", g_list_length (kvp_list));

        for (node = kvp_list;  node; node = g_list_next (node))
        {
            struct kvp_info *kvpInfo;

            kvpInfo = node->data;

            // Add to store
            add_to_store (store, text, kvpInfo);

            // Free the members and structure
            g_free (kvpInfo->kvp_path_head);
            g_free (kvpInfo->kvp_path);
            g_free (kvpInfo->match_string);
            g_free (kvpInfo->probability);
            g_free (kvpInfo);
        }
    }
    // Free the list
    g_list_free (kvp_list);
}

static void
get_account_info (ImapDialog *imap_dialog)
{
    Account *root;
    Account *acc;
    GList   *accts, *ptr;
    GtkTreeIter iter;
    GtkTreeModel *store;

    struct kvp_info kvpInfo;

    /* Get list of Accounts */
    root = gnc_book_get_root_account (gnc_get_current_book());
    accts = gnc_account_get_descendants_sorted (root);

    store = gtk_tree_view_get_model (GTK_TREE_VIEW(imap_dialog->view));
    gtk_list_store_clear (GTK_LIST_STORE(store));

    // Hide Probability Column
    show_probability_column (imap_dialog, FALSE);

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = ptr->data;

        // Save source account
        kvpInfo.source_account = acc;

        if (imap_dialog->type == BAYES)
        {
            get_imap_info (acc, NULL, store, _("Bayesian"));

            // Show Probability Column
            show_probability_column (imap_dialog, TRUE);
        }

        if (imap_dialog->type == NBAYES)
        {
            // Description
            get_imap_info (acc, IMAP_FRAME_DESC, store, _("Description Field"));

            // Memo
            get_imap_info (acc, IMAP_FRAME_MEMO, store, _("Memo Field"));

            // CSV Account Map
            get_imap_info (acc, IMAP_FRAME_CSV, store, _("CSV Account Map"));
        }

        if (imap_dialog->type == ONLINE)
        {
            gchar *text = NULL;

            kvpInfo.kvp_path = "online_id";

            text = gnc_account_get_kvp_text (acc, kvpInfo.kvp_path);

            if (text != NULL)
            {
                if (g_strcmp0 (text, "") == 0)
                    kvpInfo.map_account = NULL;
                else
                    kvpInfo.map_account = kvpInfo.source_account;

                kvpInfo.match_string  = text;
                kvpInfo.probability = " ";

                // Add kvp data to store
                add_to_store (store, _("Online Id"), &kvpInfo);
            }
            g_free (text);
        }
    }
    g_list_free (accts);
}

static void
gnc_imap_dialog_create (GtkWidget *parent, ImapDialog *imap_dialog)
{
    GtkWidget        *dialog;
    GtkBuilder       *builder;
    GtkTreeSelection *selection;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-imap-editor.glade", "list-view");
    gnc_builder_add_from_file (builder, "dialog-imap-editor.glade", "Import Map Dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Import Map Dialog"));
    imap_dialog->dialog = dialog;

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
                      G_CALLBACK(list_type_selected), (gpointer)imap_dialog);
    g_signal_connect (imap_dialog->radio_nbayes, "toggled",
                      G_CALLBACK(list_type_selected), (gpointer)imap_dialog);

    imap_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));

    /* Enable alternative line colors */
    gtk_tree_view_set_rules_hint (GTK_TREE_VIEW(imap_dialog->view), TRUE);

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
