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

void gnc_bayes_dialog_window_destroy_cb (GtkWidget *object, gpointer data);
void gnc_bayes_dialog_close_cb (GtkDialog *dialog, gpointer data);
void gnc_bayes_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer data);

static void get_account_info (BayesDialog *bayes_dialog);

void
gnc_bayes_dialog_window_destroy_cb (GtkWidget *object, gpointer data)
{
    BayesDialog *bayes_dialog = data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_BAYES_CM_CLASS, bayes_dialog);

    if (bayes_dialog->dialog)
    {
        gtk_widget_destroy(bayes_dialog->dialog);
        bayes_dialog->dialog = NULL;
    }
    g_free (bayes_dialog);
    LEAVE(" ");
}

void
gnc_bayes_dialog_close_cb (GtkDialog *dialog, gpointer data)
{
    BayesDialog *bayes_dialog = data;

    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_BAYES_CM_CLASS, bayes_dialog);
    LEAVE(" ");
}

void
gnc_bayes_dialog_response_cb (GtkDialog *dialog, gint response_id, gpointer data)
{
    BayesDialog *bayes_dialog = data;

    switch (response_id)
    {
    case GTK_RESPONSE_CLOSE:
    default:
        gnc_close_gui_component_by_data (DIALOG_BAYES_CM_CLASS, bayes_dialog);
        return;
    }
}

/** Event handler for clicking one of the list type radio buttons.
 * @param button The "List Type" radio button
 * @param bayes_dialog The display of the data
 */
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
build_bayes_layer_two (const char *key, const GValue *value, gpointer data)
{
    QofBook     *book;
    gchar       *kvp_path;
    gchar       *probability;

    struct kvp_info *kvpInfo = (struct kvp_info*)data;

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
build_bayes (const char *key, const GValue *value, gpointer data)
{
    char *kvp_path;
    struct kvp_info *kvpInfo = (struct kvp_info*)data;
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
build_non_bayes (const char *key, const GValue *value, gpointer data)
{
    if (G_VALUE_HOLDS_BOXED (value))
    {
        QofBook     *book;
        GncGUID     *guid = NULL;
        gchar       *kvp_path;

        struct kvp_info *kvpInfo = (struct kvp_info*)data;

        // Get the book
        book = gnc_get_current_book();

        guid = (GncGUID*)g_value_get_boxed (value);

        PINFO("build_non_bayes: account '%s', match account guid: '%s'", (char*)key, guid_to_string(guid));

        kvp_path = g_strconcat (kvpInfo->kvp_path_head, "/", key, NULL);

        PINFO("build_non_bayes: kvp_path is '%s'", kvp_path);

        kvpInfo->map_account = xaccAccountLookup (guid, book);
        kvpInfo->match_string = key;
        kvpInfo->kvp_path = kvp_path;
        kvpInfo->probability = " ";

        // Add kvp data to store
        add_to_store (kvpInfo);

        g_free (kvp_path);
    }
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
        gchar   *kvp_path_head = NULL;

        // Save source account
        kvpInfo.source_account = acc;

        if (bayes_dialog->type == BAYES)
        {
            if (qof_instance_has_slot (QOF_INSTANCE(acc), IMAP_FRAME_BAYES))
                qof_instance_foreach_slot(QOF_INSTANCE(acc), IMAP_FRAME_BAYES, build_bayes, &kvpInfo);
        }

        if (bayes_dialog->type == NBAYES)
        {
            kvp_path_head = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_DESC);

            kvpInfo.based_on = _("Description Field");
            kvpInfo.kvp_path_head = kvp_path_head;

            if (qof_instance_has_slot (QOF_INSTANCE(acc), kvp_path_head))
                qof_instance_foreach_slot (QOF_INSTANCE(acc), kvp_path_head, build_non_bayes, &kvpInfo);

            g_free (kvp_path_head);
            kvp_path_head = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_MEMO);

            kvpInfo.based_on = _("Memo Field");
            kvpInfo.kvp_path_head = kvp_path_head;

            if (qof_instance_has_slot (QOF_INSTANCE (acc), kvp_path_head))
                qof_instance_foreach_slot (QOF_INSTANCE(acc), kvp_path_head, build_non_bayes, &kvpInfo);

            g_free (kvp_path_head);
            kvp_path_head = g_strdup_printf (IMAP_FRAME "/%s", IMAP_FRAME_CSV);

            kvpInfo.based_on = _("CSV Account Map");
            kvpInfo.kvp_path_head = kvp_path_head;

            if (qof_instance_has_slot (QOF_INSTANCE(acc), kvp_path_head))
                qof_instance_foreach_slot (QOF_INSTANCE(acc), kvp_path_head, build_non_bayes, &kvpInfo);

            g_free (kvp_path_head);
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
    gtk_tree_view_set_rules_hint (GTK_TREE_VIEW(bayes_dialog->view),TRUE);

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
