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

#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include <gnc-glib-utils.h>
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
                     MAP_FULL_ACC, MAP_ACCOUNT, HEAD, CATEGORY, COUNT, FILTER};

typedef enum
{
    BAYES,
    NBAYES,
    ONLINE
}GncListType;

typedef struct
{
    guint inv_dialog_shown_bayes : 1;
    guint inv_dialog_shown_nbayes : 1;
    guint inv_dialog_shown_online : 1;
}GncInvFlags;

typedef struct
{
    GtkWidget    *window;
    QofSession   *session;
    GtkWidget    *view;
    GtkTreeModel *model;
    GncListType   type;

    GtkWidget    *radio_bayes;
    GtkWidget    *radio_nbayes;
    GtkWidget    *radio_online;

    GtkWidget    *filter_button;
    GtkWidget    *filter_text_entry;
    GtkWidget    *filter_label;
    gboolean      apply_selection_filter;

    GtkWidget    *total_entries_label;
    gint          tot_entries;
    gint          tot_invalid_maps;

    GtkWidget    *expand_button;
    GtkWidget    *collapse_button;
    GtkWidget    *remove_button;
    GtkWidget    *delete_button;
    GtkWidget    *close_button;

    GncInvFlags   inv_dialog_shown;
}ImapStruct;


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void get_account_info (ImapStruct *imap);
static void close_handler (gpointer user_data);

static void
gnc_imap_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    ImapStruct *imap = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_IMAP_CM_CLASS, imap);

    if (imap->window)
    {
        gtk_window_destroy (GTK_WINDOW(imap->window));
        imap->window = NULL;
    }
    g_free (imap);
    LEAVE(" ");
}

static gboolean
gnc_imap_dialog_close_event_cb (GtkWidget       *widget,
                                const GdkEvent  *event,
                                gpointer         user_data)
{
    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(widget));
    return false;
}

static void
delete_info_bayes (Account *source_account, gchar *head, gint depth)
{
    if (depth != 1) // below top level
        gnc_account_delete_map_entry (source_account, head, NULL, NULL, FALSE);
    else
        gnc_account_delete_all_bayes_maps (source_account);
}

static void
delete_info_nbayes (Account *source_account, gchar *head,
                    gchar *category, gchar *match_string, gint depth)
{
    if (depth != 1) // below top level
    {
        gnc_account_delete_map_entry (source_account, head, category, match_string, FALSE);
        gnc_account_delete_map_entry (source_account, head, category, NULL, TRUE);
    }
    else
        gnc_account_delete_map_entry (source_account, head, category, NULL, FALSE);

    gnc_account_delete_map_entry (source_account, head, NULL, NULL, TRUE);
}

static void
delete_selected_row (ImapStruct *imap, GtkTreeIter *iter)
{
    Account     *source_account = NULL;
    gchar       *full_source_account;
    gchar       *head;
    gchar       *category;
    gchar       *match_string;
    gint         num = 0;
    GtkTreeIter  parent;

    // get the parent iter and see how many children it has, if 1 we will remove
    if (gtk_tree_model_iter_parent (imap->model, &parent, iter))
        num = gtk_tree_model_iter_n_children (imap->model, &parent);

    gtk_tree_model_get (imap->model, iter, SOURCE_ACCOUNT, &source_account,
                                           SOURCE_FULL_ACC, &full_source_account,
                                           HEAD, &head,
                                           CATEGORY, &category,
                                           MATCH_STRING, &match_string, -1);

    PINFO("Account is '%s', Head is '%s', Category is '%s', Match String is '%s'",
           full_source_account, head, category, match_string);

    if (source_account != NULL)
    {
        GtkTreePath *tree_path;
        gint         depth;

        // Get the level we are at in the tree-model
        tree_path = gtk_tree_model_get_path (imap->model, iter);
        depth = gtk_tree_path_get_depth (tree_path);
        gtk_tree_path_free (tree_path);

        if (imap->type == ONLINE)
            gnc_account_delete_map_entry (source_account, head, NULL, NULL, FALSE);

        if (imap->type == BAYES)
            delete_info_bayes (source_account, head, depth);

        if (imap->type == NBAYES)
            delete_info_nbayes (source_account, head, category, match_string, depth);

        gtk_tree_store_remove (GTK_TREE_STORE(imap->model), iter);

        if (num == 1 && (imap->type != ONLINE))
            gtk_tree_store_remove (GTK_TREE_STORE(imap->model), &parent);
    }
    // Clear the total
    gtk_label_set_text (GTK_LABEL(imap->total_entries_label), " ");

    if (head)
        g_free (head);
    if (category)
        g_free (category);
    if (match_string)
        g_free (match_string);
    if (full_source_account)
        g_free (full_source_account);
}

static gboolean
find_invalid_mappings_total (GtkTreeModel *model, GtkTreePath *path,
                             GtkTreeIter *iter, ImapStruct *imap)
{
    Account *source_account = NULL;
    Account *map_account = NULL;
    gchar   *head;
    gint     depth;

    gtk_tree_model_get (model, iter, SOURCE_ACCOUNT, &source_account,
                                     MAP_ACCOUNT, &map_account,
                                     HEAD, &head, -1);

    depth = gtk_tree_path_get_depth (path);

    if ((source_account != NULL) && (map_account == NULL))
    {
        if (((g_strcmp0 (head, "online_id") == 0) && (depth == 1)) || (depth == 2))
            imap->tot_invalid_maps ++;
    }
    g_free (head);
    return FALSE;
}

static void
gnc_imap_dialog_delete (ImapStruct *imap)
{
    GList            *list, *row;
    GtkTreeModel     *fmodel;
    GtkTreeIter       fiter;
    GtkTreeIter       iter;
    GtkTreeSelection *selection;

    fmodel = gtk_tree_view_get_model (GTK_TREE_VIEW(imap->view));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(imap->view));

    list = gtk_tree_selection_get_selected_rows (selection, &fmodel);

    // Make sure we have some rows selected
    if (!gnc_list_length_cmp (list, 0))
        return;

    // reset the invalid map total
    imap->tot_invalid_maps = 0;

    // reverse list
    list = g_list_reverse (list);

    // Suspend GUI refreshing
    gnc_suspend_gui_refresh();

    // Walk the list
    for (row = g_list_first (list); row; row = g_list_next (row))
    {
        if (gtk_tree_model_get_iter (fmodel, &fiter, row->data))
        {
            gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(fmodel), &iter, &fiter);
            delete_selected_row (imap, &iter);
        }
    }
    g_list_foreach (list, (GFunc) gtk_tree_path_free, NULL);
    g_list_free (list);

    // Enable GUI refresh again
    gnc_resume_gui_refresh();

    // recount the number of invalid maps
    gtk_tree_model_foreach (imap->model,
                            (GtkTreeModelForeachFunc)find_invalid_mappings_total,
                            imap);

    if (imap->tot_invalid_maps == 0)
        gtk_widget_set_visible (GTK_WIDGET(imap->remove_button), FALSE);

}

static gboolean
find_invalid_mappings (GtkTreeModel *model, GtkTreePath *path,
                       GtkTreeIter *iter, GList **rowref_list)
{
    Account *source_account = NULL;
    Account *map_account = NULL;
    gchar   *head;
    gint     depth;

    gtk_tree_model_get (model, iter, SOURCE_ACCOUNT, &source_account,
                                     MAP_ACCOUNT, &map_account,
                                     HEAD, &head, -1);

    depth = gtk_tree_path_get_depth (path);

    if ((source_account != NULL) && (map_account == NULL))
    {
        if (((g_strcmp0 (head, "online_id") == 0) && (depth == 1)) || (depth == 2))
        {
             GtkTreeRowReference *rowref = gtk_tree_row_reference_new (model, path);
             *rowref_list = g_list_prepend (*rowref_list, rowref);
        }
    }
    g_free (head);
    return FALSE;
}

static void
gnc_imap_remove_invalid_maps (ImapStruct *imap)
{
    GList *rr_list = NULL;
    GList *node;

    gtk_tree_model_foreach (imap->model,
                            (GtkTreeModelForeachFunc)find_invalid_mappings,
                            &rr_list);

    // Suspend GUI refreshing
    gnc_suspend_gui_refresh();

    // Walk the list
    for (node = rr_list; node != NULL; node = node->next)
    {
        GtkTreePath *path = gtk_tree_row_reference_get_path ((GtkTreeRowReference*)node->data);

        if (path)
        {
            GtkTreeIter iter;

            if (gtk_tree_model_get_iter (GTK_TREE_MODEL(imap->model), &iter, path))
                delete_selected_row (imap, &iter);

            gtk_tree_path_free (path);
        }
    }

    // Enable GUI refresh again
    gnc_resume_gui_refresh();

    g_list_foreach (rr_list, (GFunc)gtk_tree_row_reference_free, NULL);
    g_list_free (rr_list);
}

static void
gnc_imap_invalid_maps_dialog (ImapStruct *imap)
{
    gtk_widget_set_visible (GTK_WIDGET(imap->remove_button), FALSE);

    if (imap->tot_invalid_maps > 0)
    {
        /* Translators: This is a ngettext(3) message, %d is the number of maps missing */
        gchar *message = g_strdup_printf (ngettext ("There is %d invalid mapping,\n\nWould you like to remove it now?",
                                                    "There are %d invalid mappings,\n\nWould you like to remove them now?",
                                                    imap->tot_invalid_maps),
                                                    imap->tot_invalid_maps);

        gchar *message2 = g_strdup_printf (gettext ("To see the invalid mappings, use a filter of '%s'"), _("Map Account NOT found"));

        gchar *text = g_strdup_printf ("%s\n\n%s\n\n%s", message, message2, _("(Note, if there is a large number, it may take a while)"));

        if (gnc_verify_dialog (GTK_WINDOW(imap->window), FALSE, "%s", text))
        {
            gnc_imap_remove_invalid_maps (imap);
            gtk_widget_set_visible (GTK_WIDGET(imap->remove_button), FALSE);
        }
        else
        {
            gtk_widget_set_visible (GTK_WIDGET(imap->remove_button), TRUE);

            if (imap->type == BAYES)
                imap->inv_dialog_shown.inv_dialog_shown_bayes = TRUE;
            if (imap->type == NBAYES)
                imap->inv_dialog_shown.inv_dialog_shown_nbayes = TRUE;
            if (imap->type == ONLINE)
                imap->inv_dialog_shown.inv_dialog_shown_online = TRUE;
        }
        g_free (message);
        g_free (message2);
        g_free (text);
    }
}

static void
response_button_cb (GtkWidget *widget, gpointer user_data)
{
    ImapStruct *imap = user_data;

    if (widget == GTK_WIDGET(imap->delete_button))
        gnc_imap_dialog_delete (imap);

    if (widget == GTK_WIDGET(imap->remove_button))
        gnc_imap_invalid_maps_dialog (imap);

    if (widget == GTK_WIDGET(imap->close_button))
        gnc_close_gui_component_by_data (DIALOG_IMAP_CM_CLASS, imap);
}

static gboolean
filter_test_and_move_next (ImapStruct *imap, GtkTreeIter *iter,
                           const gchar *filter_text)
{
    GtkTreePath *tree_path;
    gint         depth;
    gboolean     valid;
    gchar       *match_string;
    gchar       *map_full_acc;

    // Read the row
    gtk_tree_model_get (imap->model, iter, MATCH_STRING, &match_string, MAP_FULL_ACC, &map_full_acc, -1);

    // Get the level we are at in the tree-model
    tree_path = gtk_tree_model_get_path (imap->model, iter);
    depth = gtk_tree_path_get_depth (tree_path);

    // Reset filter to TRUE
    gtk_tree_store_set (GTK_TREE_STORE(imap->model), iter, FILTER, TRUE, -1);

    // Check for a filter_text entry
    if (filter_text && *filter_text != '\0')
    {
        if (match_string != NULL) // Check for match_string is not NULL, valid line
        {
            if ((g_strrstr (match_string, filter_text) == NULL) &&
                (g_strrstr (map_full_acc, filter_text) == NULL ))
                gtk_tree_store_set (GTK_TREE_STORE(imap->model), iter, FILTER, FALSE, -1);
            else
                gtk_tree_view_expand_to_path (GTK_TREE_VIEW(imap->view), tree_path);
        }
    }
    // Select next entry based on path
    if (depth == 1)
        gtk_tree_path_down (tree_path);
    else
    {
        gtk_tree_path_next (tree_path);
        if (!gtk_tree_model_get_iter (imap->model, iter, tree_path))
        {
            gtk_tree_path_prev (tree_path);
            gtk_tree_path_up (tree_path);
            gtk_tree_path_next (tree_path);
        }
    }
    valid = gtk_tree_model_get_iter (imap->model, iter, tree_path);

    gtk_tree_path_free (tree_path);
    g_free (match_string);
    g_free (map_full_acc);

    return valid;
}

static void
filter_button_cb (GtkButton *button, ImapStruct *imap)
{
    GtkTreeIter   iter;
    gboolean      valid;
    const gchar  *filter_text = gnc_entry_get_text (GTK_ENTRY(imap->filter_text_entry));

    // Collapse all nodes
    gtk_tree_view_collapse_all (GTK_TREE_VIEW(imap->view));
    imap->apply_selection_filter = FALSE;

    // clear any selection
    gtk_tree_selection_unselect_all (gtk_tree_view_get_selection
                                    (GTK_TREE_VIEW(imap->view)));

    // do we have a filter, apply selection filter
    if (filter_text && *filter_text != '\0')
        imap->apply_selection_filter = TRUE;

    valid = gtk_tree_model_get_iter_first (imap->model, &iter);

    while (valid)
    {
        valid = filter_test_and_move_next (imap, &iter, filter_text);
    }
    gtk_widget_grab_focus (GTK_WIDGET(imap->view));
}

static void
expand_button_cb (GtkButton *button, ImapStruct *imap)
{
    // Clear the filter
    gnc_entry_set_text (GTK_ENTRY(imap->filter_text_entry), "");

    filter_button_cb (button, imap);

    gtk_tree_view_expand_all (GTK_TREE_VIEW(imap->view));
}

static void
collapse_button_cb (GtkButton *button, ImapStruct *imap)
{
    // Clear the filter
    gnc_entry_set_text (GTK_ENTRY(imap->filter_text_entry), "");

    filter_button_cb (button, imap);

    gtk_tree_view_collapse_all (GTK_TREE_VIEW(imap->view));
}

static void
list_type_selected_cb (GtkCheckButton* button, ImapStruct *imap)
{
    GncListType type;

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(imap->radio_bayes)))
        type = BAYES;
    else if (gtk_check_button_get_active (GTK_CHECK_BUTTON(imap->radio_nbayes)))
        type = NBAYES;
    else
        type = ONLINE;

    if (type != ONLINE)
        gtk_widget_grab_focus (GTK_WIDGET(imap->filter_text_entry));

    // Lets do this only on change of list type
    if (type != imap->type)
    {
        gboolean inv_dialog_shown = FALSE;

        imap->type = type;
        get_account_info (imap);

        if ((imap->type == BAYES) && (imap->inv_dialog_shown.inv_dialog_shown_bayes))
            inv_dialog_shown = TRUE;

        if ((imap->type == NBAYES) && (imap->inv_dialog_shown.inv_dialog_shown_nbayes))
            inv_dialog_shown = TRUE;

        if ((imap->type == ONLINE) && (imap->inv_dialog_shown.inv_dialog_shown_online))
            inv_dialog_shown = TRUE;

        if (!inv_dialog_shown)
            gnc_imap_invalid_maps_dialog (imap);
    }
}

static void
show_count_column (ImapStruct *imap, gboolean show)
{
    GtkTreeViewColumn *tree_column;

    // Show Count Column
    tree_column = gtk_tree_view_get_column (GTK_TREE_VIEW(imap->view), 4);
    gtk_tree_view_column_set_visible (tree_column, show);

    // Hide Based on Column
    tree_column = gtk_tree_view_get_column (GTK_TREE_VIEW(imap->view), 1);
    gtk_tree_view_column_set_visible (tree_column, !show);

    gtk_tree_view_columns_autosize (GTK_TREE_VIEW(imap->view));
}

static void
add_to_store (ImapStruct *imap, GtkTreeIter *iter, const gchar *text, GncImapInfo *imapInfo)
{
    gchar *fullname = NULL;
    gchar *map_fullname = NULL;

    fullname = gnc_account_get_full_name (imapInfo->source_account);

    // Do we have a valid map account
    if (imapInfo->map_account == NULL)
    {
        // count the total invalid maps
        imap->tot_invalid_maps ++;

        map_fullname = g_strdup (_("Map Account NOT found"));
    }
    else
        map_fullname = gnc_account_get_full_name (imapInfo->map_account);

    // count the total entries
    imap->tot_entries ++;

    PINFO("Add to Store: Source Acc '%s', Head is '%s', Category is '%s', Match '%s', Map Acc '%s', Count is %s",
          fullname, imapInfo->head, imapInfo->category, imapInfo->match_string, map_fullname, imapInfo->count);

    gtk_tree_store_set (GTK_TREE_STORE(imap->model), iter,
                        SOURCE_FULL_ACC, fullname, SOURCE_ACCOUNT, imapInfo->source_account,
                        BASED_ON, text,
                        MATCH_STRING, imapInfo->match_string,
                        MAP_FULL_ACC, map_fullname, MAP_ACCOUNT, imapInfo->map_account,
                        HEAD, imapInfo->head, CATEGORY, imapInfo->category, COUNT, imapInfo->count,
                        FILTER, TRUE, -1);

    g_free (fullname);
    g_free (map_fullname);
}

static void
get_imap_info (ImapStruct *imap, Account *acc, const gchar *category, const gchar *text)
{
    GtkTreeIter  toplevel, child;
    GList *imap_list, *node;
    gchar *acc_name = NULL;
    gchar *head = NULL;

    acc_name = gnc_account_get_full_name (acc);
    PINFO("Source Acc '%s', Based on '%s', Path Head '%s'", acc_name, text, category);

    if (category == NULL) // For Bayesian, category is NULL
        imap_list = gnc_account_imap_get_info_bayes (acc);
    else
        imap_list = gnc_account_imap_get_info (acc, category);

    if (category == NULL)
        head = IMAP_FRAME_BAYES;
    else
        head = IMAP_FRAME;

    if (gnc_list_length_cmp (imap_list, 0))
    {
        PINFO("List length is %d", g_list_length (imap_list));

        // Add top level entry of Source full Account and Based on.
        gtk_tree_store_append (GTK_TREE_STORE(imap->model), &toplevel, NULL);
        gtk_tree_store_set (GTK_TREE_STORE(imap->model), &toplevel,
                            SOURCE_ACCOUNT, acc,
                            SOURCE_FULL_ACC, acc_name,
                            HEAD, head,
                            CATEGORY, category,
                            BASED_ON, text,
                            FILTER, TRUE, -1);

        for (node = imap_list;  node; node = g_list_next (node))
        {
            GncImapInfo *imapInfo = node->data;

            // First add a child entry and pass iter to add_to_store
            gtk_tree_store_append (GTK_TREE_STORE(imap->model), &child, &toplevel);
            add_to_store (imap, &child, text, imapInfo);
        }
    }
    g_free (acc_name);
    g_list_free_full (imap_list, (GDestroyNotify)gnc_account_imap_info_destroy); // Free the List
}

static void
show_first_row (ImapStruct *imap)
{
    GtkTreeIter   iter;

    // See if there are any entries
    if (gtk_tree_model_get_iter_first (imap->model, &iter))
    {
        GtkTreePath *path;
        path = gtk_tree_path_new_first (); // Set Path to first entry
        gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(imap->view), path, NULL, TRUE, 0.0, 0.0);
        gtk_tree_path_free (path);
    }
}

static void
get_account_info_bayes (ImapStruct *imap, GList *accts)
{
    GList   *ptr;

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = ptr->data;

        get_imap_info (imap, acc, NULL, _("Bayesian"));
    }
}

static void
get_account_info_nbayes (ImapStruct *imap, GList *accts)
{
    GList   *ptr;

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = ptr->data;

        // Description
        get_imap_info (imap, acc, IMAP_FRAME_DESC, _("Description Field"));

        // Memo
        get_imap_info (imap, acc, IMAP_FRAME_MEMO, _("Memo Field"));

        // CSV Account Map
        get_imap_info (imap, acc, IMAP_FRAME_CSV, _("CSV Account Map"));
    }
}

static void
get_account_info_online (ImapStruct *imap, GList *accts)
{
    GList       *ptr;
    GtkTreeIter  toplevel;

    GncImapInfo imapInfo;

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        gchar  *hbci_account_id = NULL;
        gchar  *hbci_bank_code = NULL;
        gchar  *text = NULL;
        Account *acc = ptr->data;

        // Check for online_id
        text = gnc_account_get_map_entry (acc, "online_id", NULL);

        if (text != NULL)
        {
            // Save source account
            imapInfo.source_account = acc;
            imapInfo.head = "online_id";
            imapInfo.category = " ";

            if (g_strcmp0 (text, "") == 0)
                imapInfo.map_account = NULL;
            else
                imapInfo.map_account = imapInfo.source_account;

            imapInfo.match_string = text;
            imapInfo.count = " ";

            // Add top level entry and pass iter to add_to_store
            gtk_tree_store_append (GTK_TREE_STORE(imap->model), &toplevel, NULL);
            add_to_store (imap, &toplevel, _("Online Id"), &imapInfo);
        }
        g_free (text);

        // Check for aqbanking hbci
        hbci_account_id = gnc_account_get_map_entry (acc, "hbci", "account-id");
        hbci_bank_code = gnc_account_get_map_entry (acc, "hbci", "bank-code");
        text = g_strconcat (hbci_bank_code, ",", hbci_account_id, NULL);

        if ((hbci_account_id != NULL) || (hbci_bank_code != NULL))
        {
            // Save source account
            imapInfo.source_account = acc;
            imapInfo.head = "hbci";
            imapInfo.category = " ";

            if (g_strcmp0 (text, "") == 0)
                imapInfo.map_account = NULL;
            else
                imapInfo.map_account = imapInfo.source_account;

            imapInfo.match_string = text;
            imapInfo.count = " ";

            // Add top level entry and pass iter to add_to_store
            gtk_tree_store_append (GTK_TREE_STORE(imap->model), &toplevel, NULL);
            add_to_store (imap, &toplevel, _("Online HBCI"), &imapInfo);
        }
        g_free (hbci_account_id);
        g_free (hbci_bank_code);
        g_free (text);
    }
}

static void
show_filter_option (ImapStruct *imap, gboolean show)
{
    gtk_widget_set_visible (GTK_WIDGET(imap->filter_text_entry), show);
    gtk_widget_set_visible (GTK_WIDGET(imap->filter_button), show);
    gtk_widget_set_visible (GTK_WIDGET(imap->filter_label), show);
    gtk_widget_set_visible (GTK_WIDGET(imap->expand_button), show);
    gtk_widget_set_visible (GTK_WIDGET(imap->collapse_button), show);
}

static void
get_account_info (ImapStruct *imap)
{
    Account      *root;
    GList        *accts;
    GtkTreeModel *fmodel;
    gchar        *total;

    /* Get list of Accounts */
    root = gnc_book_get_root_account (gnc_get_current_book());
    accts = gnc_account_get_descendants_sorted (root);

    imap->tot_entries = 0;
    imap->tot_invalid_maps = 0;

    fmodel = gtk_tree_view_get_model (GTK_TREE_VIEW(imap->view));

    imap->model = gtk_tree_model_filter_get_model (GTK_TREE_MODEL_FILTER(fmodel));

    // Disconnect the filter model from the treeview
    g_object_ref (G_OBJECT(imap->model));
    gtk_tree_view_set_model (GTK_TREE_VIEW(imap->view), NULL);

    // Clear the tree store
    gtk_tree_store_clear (GTK_TREE_STORE(imap->model));

    // Clear the filter
    gnc_entry_set_text (GTK_ENTRY(imap->filter_text_entry), "");
    imap->apply_selection_filter = FALSE;

    // Hide Count Column
    show_count_column (imap, FALSE);

    // Show Filter Option
    show_filter_option (imap, TRUE);

    if (imap->type == BAYES)
    {
        get_account_info_bayes (imap, accts);

        // Show Count Column
        show_count_column (imap, TRUE);
    }
    else if (imap->type == NBAYES)
        get_account_info_nbayes (imap, accts);
    else if (imap->type == ONLINE)
    {
        // Hide Filter Option
        show_filter_option (imap, FALSE);
        get_account_info_online (imap, accts);
    }
    // create a new filter model and reconnect to treeview
    fmodel = gtk_tree_model_filter_new (GTK_TREE_MODEL(imap->model), NULL);
    gtk_tree_model_filter_set_visible_column (GTK_TREE_MODEL_FILTER(fmodel), FILTER);
    g_object_unref (G_OBJECT(imap->model));

    gtk_tree_view_set_model (GTK_TREE_VIEW(imap->view), fmodel);
    g_object_unref (G_OBJECT(fmodel));

    // if there are any entries, show first row
    show_first_row (imap);

    // add the totals
    total = g_strdup_printf ("%s %d", _("Total Entries"), imap->tot_entries);
    gtk_label_set_text (GTK_LABEL(imap->total_entries_label), total);
    gtk_widget_set_visible (GTK_WIDGET(imap->total_entries_label), TRUE);
    g_free (total);

    gtk_widget_set_visible (GTK_WIDGET(imap->remove_button),
                            (imap->tot_invalid_maps > 0));

    g_list_free (accts);
}

static gboolean
view_selection_function (GtkTreeSelection *selection,
                         GtkTreeModel *model,
                         GtkTreePath *path,
                         gboolean path_currently_selected,
                         gpointer user_data)
{
    ImapStruct *imap = user_data;
    GtkTreeIter iter;

    if (!imap->apply_selection_filter)
        return TRUE;

    // do we have a valid row
    if (gtk_tree_model_get_iter (model, &iter, path))
    {
        gchar *match_string;

        // read the row
        gtk_tree_model_get (model, &iter, MATCH_STRING, &match_string, -1);

        // match_string NULL, top level can not be selected with a filter
        if (match_string == NULL)
            return FALSE;
        g_free (match_string);
    }
    return TRUE;
}

static gboolean
gnc_imap_dialog_key_press_cb (GtkEventControllerKey *key, guint keyval,
                              guint keycode, GdkModifierType state,
                              gpointer user_data)
{
    ImapStruct *imap = user_data;

    if (keyval == GDK_KEY_Escape)
    {
        if (gnc_ok_to_close_window (GTK_WIDGET(imap->window)))
            close_handler (imap);

        return TRUE;
    }
    else
        return FALSE;
}

static void
gnc_imap_dialog_create (GtkWidget *parent, ImapStruct *imap)
{
    GtkBuilder       *builder;
    GtkTreeModel     *filter;
    GtkTreeSelection *selection;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-imap-editor.ui", "tree-store");
    gnc_builder_add_from_file (builder, "dialog-imap-editor.ui", "treemodelfilter");
    gnc_builder_add_from_file (builder, "dialog-imap-editor.ui", "import_map_window");

    imap->window = GTK_WIDGET(gtk_builder_get_object (builder, "import_map_window"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(imap->window), "gnc-id-import-map");

    imap->session = gnc_get_current_session();
    imap->type = BAYES;

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(imap->window), GTK_WINDOW(parent));

    /* Connect the radio buttons...*/
    imap->radio_bayes = GTK_WIDGET(gtk_builder_get_object (builder, "radio-bayes"));
    imap->radio_nbayes = GTK_WIDGET(gtk_builder_get_object (builder, "radio-nbayes"));
    imap->radio_online = GTK_WIDGET(gtk_builder_get_object (builder, "radio-online"));
    g_signal_connect (G_OBJECT(imap->radio_bayes), "toggled",
                      G_CALLBACK(list_type_selected_cb), (gpointer)imap);
    g_signal_connect (G_OBJECT(imap->radio_nbayes), "toggled",
                      G_CALLBACK(list_type_selected_cb), (gpointer)imap);

    imap->total_entries_label = GTK_WIDGET(gtk_builder_get_object (builder, "total_entries_label"));
    imap->filter_text_entry = GTK_WIDGET(gtk_builder_get_object (builder, "filter-text-entry"));
    imap->filter_label = GTK_WIDGET(gtk_builder_get_object (builder, "filter-label"));
    imap->filter_button = GTK_WIDGET(gtk_builder_get_object (builder, "filter-button"));
    g_signal_connect (G_OBJECT(imap->filter_button), "clicked",
                      G_CALLBACK(filter_button_cb), (gpointer)imap);

    imap->expand_button = GTK_WIDGET(gtk_builder_get_object (builder, "expand-button"));
    g_signal_connect (G_OBJECT(imap->expand_button), "clicked",
                      G_CALLBACK(expand_button_cb), (gpointer)imap);

    imap->collapse_button = GTK_WIDGET(gtk_builder_get_object (builder, "collapse-button"));
    g_signal_connect (G_OBJECT(imap->collapse_button), "clicked",
                      G_CALLBACK(collapse_button_cb), (gpointer)imap);

    imap->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));

    imap->delete_button = GTK_WIDGET(gtk_builder_get_object (builder, "delete_button"));
    g_signal_connect (G_OBJECT(imap->delete_button), "clicked",
                      G_CALLBACK(response_button_cb), imap);
    imap->close_button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
    g_signal_connect (G_OBJECT(imap->close_button), "clicked",
                      G_CALLBACK(response_button_cb), imap);
    imap->remove_button = GTK_WIDGET(gtk_builder_get_object (builder, "remove_button"));
    g_signal_connect (G_OBJECT(imap->remove_button), "clicked",
                      G_CALLBACK(response_button_cb), imap);

    /* default to 'close' button */
    gtk_window_set_default_widget (GTK_WINDOW(imap->window),
                                   GTK_WIDGET(imap->close_button)); //FIXME gtk4, may not work

    // Set filter column
    filter = gtk_tree_view_get_model (GTK_TREE_VIEW(imap->view));
    gtk_tree_model_filter_set_visible_column (GTK_TREE_MODEL_FILTER(filter), FILTER);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(imap->view), gnc_tree_view_get_grid_lines_pref ());

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(imap->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    /* add a select function  */
    gtk_tree_selection_set_select_function (selection,
                                            view_selection_function,
                                            imap,
                                            NULL);

    g_signal_connect (G_OBJECT(imap->window), "destroy",
                      G_CALLBACK(gnc_imap_dialog_window_destroy_cb), imap);

    g_signal_connect (G_OBJECT(imap->window), "close-request",
                      G_CALLBACK(gnc_imap_dialog_close_event_cb), NULL);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(imap->window), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window),
                      "key-pressed",
                      G_CALLBACK(gnc_imap_dialog_key_press_cb), imap);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(imap->window), GTK_WINDOW(parent));

    gtk_widget_set_visible (GTK_WIDGET(imap->window), TRUE);

    get_account_info (imap);

    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    ImapStruct *imap = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(imap->window));
    gtk_window_destroy (GTK_WINDOW(imap->window));
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
    ImapStruct *imap = user_data;

    ENTER(" ");
    if (!imap)
    {
        LEAVE("No data structure");
        return(FALSE);
    }
    gtk_window_present (GTK_WINDOW(imap->window));
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
    ImapStruct *imap;
    gint component_id;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_IMAP_CM_CLASS, show_handler, NULL))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    imap = g_new0 (ImapStruct, 1);

    gnc_imap_dialog_create (parent, imap);

    component_id = gnc_register_gui_component (DIALOG_IMAP_CM_CLASS,
                   refresh_handler, close_handler,
                   imap);

    gnc_gui_component_set_session (component_id, imap->session);

    gtk_widget_set_visible (GTK_WIDGET(imap->window), TRUE);
    gtk_widget_set_visible (GTK_WIDGET(imap->remove_button), FALSE);
    gnc_imap_invalid_maps_dialog (imap);
    LEAVE(" ");
}
