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
