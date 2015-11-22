/********************************************************************\
 * dialog-bayes-editor.c -- Bayesian and Non Bayesian editor dialog *
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

#include "dialog-bayes-editor.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"

#include "gnc-ui-util.h"
#include "qofinstance-p.h"

#define DIALOG_BAYES_CM_CLASS   "dialog-bayes-edit"
#define GNC_PREFS_GROUP         "dialogs.bayes-editor"

#define IMAP_FRAME_BAYES        "import-map-bayes"
#define IMAP_FRAME              "import-map"

#define IMAP_FRAME_DESC         "desc"
#define IMAP_FRAME_MEMO         "memo"
#define IMAP_FRAME_CSV          "csv-account-map"

/** Enumeration for the liststore */
enum GncBayesColumn {SOURCE_FULL_ACC, SOURCE_ACCOUNT, BASED_ON, MATCH_STRING,
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

    GtkWidget *radio_bayes;
    GtkWidget *radio_nbayes;
    GtkWidget *radio_online;

} BayesDialog;

struct kvp_info
{
    GtkTreeModel *store;
    Account      *source_account;
    Account      *map_account;
    const gchar  *based_on;
    const gchar  *match_string;
    const gchar  *kvp_path_head;
    const gchar  *kvp_path;
    const gchar  *probability;
};


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

void gnc_bayes_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data);
void gnc_bayes_dialog_close_cb (GtkDialog *dialog, gpointer user_data);
void gnc_bayes_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer user_data);

static void get_account_info (BayesDialog *bayes_dialog);

void
gnc_bayes_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    BayesDialog *bayes_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_BAYES_CM_CLASS, bayes_dialog);

    if (bayes_dialog->dialog)
    {
        gtk_widget_destroy (bayes_dialog->dialog);
        bayes_dialog->dialog = NULL;
    }
    g_free (bayes_dialog);
    LEAVE(" ");
}

void
gnc_bayes_dialog_close_cb (GtkDialog *dialog, gpointer user_data)
{
    BayesDialog *bayes_dialog = user_data;

    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_BAYES_CM_CLASS, bayes_dialog);
    LEAVE(" ");
}

static gboolean
are_you_sure (BayesDialog *bayes_dialog)
{
    GtkWidget   *dialog;
    gint         response;
    const char  *title = _("Are you sure you want to delete the entries ?");

    dialog = gtk_message_dialog_new (GTK_WINDOW (bayes_dialog->dialog),
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
delete_kvp (Account *account, gchar *full_account, gchar *kvp_path)
{
    qof_instance_slot_delete_if_empty (QOF_INSTANCE(account), kvp_path);

    PINFO("Delete source account is '%s', path is '%s'", full_account, kvp_path);

    g_free (kvp_path);
}

static void
gnc_bayes_dialog_delete (BayesDialog *bayes_dialog)
{
    GList            *list, *row;
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    GtkTreeSelection *selection;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(bayes_dialog->view));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(bayes_dialog->view));

    list = gtk_tree_selection_get_selected_rows (selection, &model);

    // Make sure we have some rows selected
    if (g_list_length (list) == 0)
        return;

    // Are we sure we want to delete the entries
    if (are_you_sure (bayes_dialog) == FALSE)
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

            if ((source_account != NULL) && qof_instance_has_slot (QOF_INSTANCE(source_account), kvp_path))
            {
                xaccAccountBeginEdit (source_account);

                qof_instance_slot_delete (QOF_INSTANCE(source_account), kvp_path);
                g_free (kvp_path);

                if (bayes_dialog->type == BAYES)
                {
                    kvp_path = g_strdup_printf (IMAP_FRAME_BAYES "/%s", match_string);
                    delete_kvp (source_account, full_source_account, kvp_path);

                    kvp_path = g_strdup_printf (IMAP_FRAME_BAYES);
                    delete_kvp (source_account, full_source_account, kvp_path);
                }

                if (bayes_dialog->type == NBAYES)
                {
                    kvp_path = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_DESC);
                    delete_kvp (source_account, full_source_account, kvp_path);

                    kvp_path = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_MEMO);
                    delete_kvp (source_account, full_source_account, kvp_path);

                    kvp_path = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_CSV);
                    delete_kvp (source_account, full_source_account, kvp_path);

                    kvp_path = g_strdup_printf (IMAP_FRAME);
                    delete_kvp (source_account, full_source_account, kvp_path);
                }
                qof_instance_set_dirty (QOF_INSTANCE(source_account));
                xaccAccountCommitEdit (source_account);
            }
            g_free (match_string);
            g_free (full_source_account);
	}
    }
    g_list_foreach (list, (GFunc) gtk_tree_path_free, NULL);
    g_list_free (list);

    get_account_info (bayes_dialog);
}

void
gnc_bayes_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer user_data)
{
    BayesDialog *bayes_dialog = user_data;

    switch (response_id)
    {
    case GTK_RESPONSE_APPLY:
        gnc_bayes_dialog_delete (bayes_dialog);
        return;

    case GTK_RESPONSE_CLOSE:
    default:
        gnc_close_gui_component_by_data (DIALOG_BAYES_CM_CLASS, bayes_dialog);
        return;
    }
}

static void
list_type_selected (GtkToggleButton* button, BayesDialog *bayes_dialog)
{
    GncListType type;

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(bayes_dialog->radio_bayes)))
        type = BAYES;
    else if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(bayes_dialog->radio_nbayes)))
        type = NBAYES;
    else
        type = ONLINE;

    // Lets do this only on change of list type
    if (type != bayes_dialog->type)
    {
        bayes_dialog->type = type;
        get_account_info (bayes_dialog);
    }
}

static void
show_probability_column (BayesDialog *bayes_dialog, gboolean show)
{
    GtkTreeViewColumn *tree_column;

    // Show Probability Column
    tree_column = gtk_tree_view_get_column (GTK_TREE_VIEW(bayes_dialog->view), 4);
    gtk_tree_view_column_set_visible (tree_column, show);

    // Hide Based on Column
    tree_column = gtk_tree_view_get_column (GTK_TREE_VIEW(bayes_dialog->view), 1);
    gtk_tree_view_column_set_visible (tree_column, !show);
}

static void
add_to_store (gpointer user_data)
{
    GtkTreeIter  iter;
    gchar       *fullname = NULL;
    gchar       *map_fullname = NULL;

    struct kvp_info *kvpInfo = (struct kvp_info*)user_data;

    gtk_list_store_append (GTK_LIST_STORE(kvpInfo->store), &iter);

    fullname = gnc_account_get_full_name (kvpInfo->source_account);

    map_fullname = gnc_account_get_full_name (kvpInfo->map_account);

    PINFO("Add to Store: Source Acc '%s', Based on '%s', Map Acc '%s'", fullname, kvpInfo->based_on, map_fullname);

    gtk_list_store_set (GTK_LIST_STORE(kvpInfo->store), &iter,
                        SOURCE_FULL_ACC, fullname, SOURCE_ACCOUNT, kvpInfo->source_account,
                        BASED_ON, kvpInfo->based_on,
                        MATCH_STRING, kvpInfo->match_string,
                        MAP_FULL_ACC, map_fullname, MAP_ACCOUNT, kvpInfo->map_account,
                        KVP_PATH, kvpInfo->kvp_path, PROBABILITY, kvpInfo->probability, -1);

    g_free (fullname);
    g_free (map_fullname);
}

static void
build_bayes_layer_two (const char *key, const GValue *value, gpointer user_data)
{
    QofBook     *book;
    gchar       *kvp_path;
    gchar       *probability;

    struct kvp_info *kvpInfo = (struct kvp_info*)user_data;

    // Get the book
    book = gnc_get_current_book();

    PINFO("build_bayes_layer_two: account '%s', token_count: '%ld'", (char*)key, (long)g_value_get_int64(value));

    probability = g_strdup_printf ("%ld", g_value_get_int64 (value));

    kvp_path = g_strconcat (kvpInfo->kvp_path_head, "/", key, NULL);

    PINFO("build_bayes_layer_two: kvp_path is '%s'", kvp_path);

    kvpInfo->map_account = gnc_account_lookup_by_full_name (gnc_book_get_root_account (book), key);

    kvpInfo->kvp_path = kvp_path;
    kvpInfo->probability = probability;

    // Add kvp data to store
    add_to_store (kvpInfo);

    g_free (kvp_path);
    g_free (probability);
}

static void
build_bayes (const char *key, const GValue *value, gpointer user_data)
{
    char *kvp_path;
    struct kvp_info *kvpInfo = (struct kvp_info*)user_data;
    struct kvp_info  kvpInfol2;

    PINFO("build_bayes: match string '%s'", (char*)key);

    if (G_VALUE_HOLDS (value, G_TYPE_STRING) && g_value_get_string (value) == NULL)
    {
        kvp_path = g_strdup_printf (IMAP_FRAME_BAYES "/%s", key);

        if (qof_instance_has_slot (QOF_INSTANCE(kvpInfo->source_account), kvp_path))
        {
            PINFO("build_bayes: kvp_path is '%s', key '%s'", kvp_path, key);

            kvpInfol2.store = kvpInfo->store;
            kvpInfol2.source_account = kvpInfo->source_account;
            kvpInfol2.based_on = _("Bayesian");
            kvpInfol2.match_string = key;
            kvpInfol2.kvp_path_head = kvp_path;

            qof_instance_foreach_slot (QOF_INSTANCE(kvpInfo->source_account), kvp_path,
                                       build_bayes_layer_two, &kvpInfol2);
        }
        g_free (kvp_path);
    }
}

static void
build_non_bayes (const char *key, const GValue *value, gpointer user_data)
{
    if (G_VALUE_HOLDS_BOXED (value))
    {
        QofBook     *book;
        GncGUID     *guid = NULL;
        gchar       *kvp_path;
        gchar       *guid_string = NULL;

        struct kvp_info *kvpInfo = (struct kvp_info*)user_data;

        // Get the book
        book = gnc_get_current_book();

        guid = (GncGUID*)g_value_get_boxed (value);
        guid_string = guid_to_string (guid);

        PINFO("build_non_bayes: account '%s', match account guid: '%s'", (char*)key, guid_string);

        kvp_path = g_strconcat (kvpInfo->kvp_path_head, "/", key, NULL);

        PINFO("build_non_bayes: kvp_path is '%s'", kvp_path);

        kvpInfo->map_account = xaccAccountLookup (guid, book);
        kvpInfo->match_string = key;
        kvpInfo->kvp_path = kvp_path;
        kvpInfo->probability = " ";

        // Add kvp data to store
        add_to_store (kvpInfo);

        g_free (kvp_path);
        g_free (guid_string);
    }
}

static void
get_non_bayes_info (Account *acc, const gchar *imap_frame, GtkTreeModel *store, const gchar *text)
{
    gchar   *kvp_path_head = NULL;

    struct kvp_info kvpInfo;

    kvpInfo.source_account = acc;
    kvpInfo.store = store;
    kvpInfo.based_on = text;

    kvp_path_head = g_strdup_printf (IMAP_FRAME "/%s", imap_frame);
    kvpInfo.kvp_path_head = kvp_path_head;

    if (qof_instance_has_slot (QOF_INSTANCE(acc), kvp_path_head))
        qof_instance_foreach_slot (QOF_INSTANCE(acc), kvp_path_head, build_non_bayes, &kvpInfo);

    g_free (kvp_path_head);
}

static void
get_account_info (BayesDialog *bayes_dialog)
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

    store = gtk_tree_view_get_model (GTK_TREE_VIEW(bayes_dialog->view));
    gtk_list_store_clear (GTK_LIST_STORE(store));
    kvpInfo.store = store;

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = ptr->data;

        // Save source account
        kvpInfo.source_account = acc;

        if (bayes_dialog->type == BAYES)
        {
            if (qof_instance_has_slot (QOF_INSTANCE(acc), IMAP_FRAME_BAYES))
                qof_instance_foreach_slot (QOF_INSTANCE(acc), IMAP_FRAME_BAYES, build_bayes, &kvpInfo);

            // Show Probability Column
            show_probability_column (bayes_dialog, TRUE);
        }

        if (bayes_dialog->type == NBAYES)
        {
            // Description
            get_non_bayes_info (acc, IMAP_FRAME_DESC, store, _("Description Field"));

            // Memo
            get_non_bayes_info (acc, IMAP_FRAME_MEMO, store, _("Memo Field"));

            // CSV Account Map
            get_non_bayes_info (acc, IMAP_FRAME_CSV, store, _("CSV Account Map"));

            // Hide Probability Column
            show_probability_column (bayes_dialog, FALSE);
        }

        if (bayes_dialog->type == ONLINE)
        {
            GValue v = G_VALUE_INIT;

            kvpInfo.based_on = _("Online Id");
            kvpInfo.kvp_path = "online_id";

            if (qof_instance_has_slot (QOF_INSTANCE(acc), kvpInfo.kvp_path))
            {
                qof_instance_get_kvp (QOF_INSTANCE(acc), kvpInfo.kvp_path, &v);

                if (G_VALUE_HOLDS_STRING (&v))
                {
                    const gchar *string = g_value_get_string (&v);

                    kvpInfo.map_account = kvpInfo.source_account;
                    kvpInfo.match_string  = string;
                    kvpInfo.probability = " ";

                    // Add kvp data to store
                    add_to_store (&kvpInfo);
                }
            }
            // Hide Probability Column
            show_probability_column (bayes_dialog, FALSE);
        }
    }
    g_list_free (accts);
}

static void
gnc_bayes_dialog_create (GtkWidget *parent, BayesDialog *bayes_dialog)
{
    GtkWidget        *dialog;
    GtkBuilder       *builder;
    GtkTreeSelection *selection;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-bayes-editor.glade", "list-view");
    gnc_builder_add_from_file (builder, "dialog-bayes-editor.glade", "Bayesian Dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Bayesian Dialog"));
    bayes_dialog->dialog = dialog;

    bayes_dialog->session = gnc_get_current_session();
    bayes_dialog->type = BAYES;

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    /* Connect the radio buttons...*/
    bayes_dialog->radio_bayes = GTK_WIDGET(gtk_builder_get_object (builder, "radio-bayes"));
    bayes_dialog->radio_nbayes = GTK_WIDGET(gtk_builder_get_object (builder, "radio-nbayes"));
    bayes_dialog->radio_online = GTK_WIDGET(gtk_builder_get_object (builder, "radio-online"));
    g_signal_connect (bayes_dialog->radio_bayes, "toggled",
                      G_CALLBACK(list_type_selected), (gpointer)bayes_dialog);
    g_signal_connect (bayes_dialog->radio_nbayes, "toggled",
                      G_CALLBACK(list_type_selected), (gpointer)bayes_dialog);

    bayes_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));

    /* Enable alternative line colors */
    gtk_tree_view_set_rules_hint (GTK_TREE_VIEW(bayes_dialog->view), TRUE);

    /* default to 'close' button */
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_CLOSE);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(bayes_dialog->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, bayes_dialog);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(bayes_dialog->dialog));
    get_account_info (bayes_dialog);

    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    BayesDialog *bayes_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(bayes_dialog->dialog));
    gtk_widget_destroy (GTK_WIDGET(bayes_dialog->dialog));
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
    BayesDialog *bayes_dialog = user_data;

    ENTER(" ");
    if (!bayes_dialog)
    {
        LEAVE("No data strucure");
        return(FALSE);
    }
    gtk_window_present (GTK_WINDOW(bayes_dialog->dialog));
    LEAVE(" ");
    return(TRUE);
}

/********************************************************************\
 * gnc_bayes_dialog                                                 *
 * opens a window showing all Bayesian and Non-Bayesian information *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_bayes_dialog (GtkWidget *parent)
{
    BayesDialog *bayes_dialog;
    gint component_id;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_BAYES_CM_CLASS, show_handler, NULL))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    bayes_dialog = g_new0 (BayesDialog, 1);

    gnc_bayes_dialog_create (parent, bayes_dialog);

    component_id = gnc_register_gui_component (DIALOG_BAYES_CM_CLASS,
                   refresh_handler, close_handler,
                   bayes_dialog);

    gnc_gui_component_set_session (component_id, bayes_dialog->session);

    gtk_widget_show (bayes_dialog->dialog);
    LEAVE(" ");
}
