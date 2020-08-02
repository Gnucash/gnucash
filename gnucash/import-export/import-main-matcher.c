/********************************************************************\
 * import-main-matcher.c - Transaction matcher main window          *
 *                                                                  *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
 * Copyright (C) 2002 Christian Stimming                            *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (C) 2012 Robert Fewell                                 *
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
/** @addtogroup Import_Export
    @{ */
/** @internal
    @file import-main-matcher.c
    @brief Transaction matcher main window
    @author Copyright (C) 2002 Benoit Grégoire
    @author Christian Stimming
    @author Copyright (c) 2006 David Hampton <hampton@employees.org>
*/
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "import-main-matcher.h"

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-engine.h"
#include "gnc-gtk-utils.h"
#include "import-settings.h"
#include "import-match-picker.h"
#include "import-backend.h"
#include "import-account-matcher.h"
#include "import-pending-matches.h"
#include "gnc-component-manager.h"
#include "guid.h"
#include "gnc-session.h"
#include "Query.h"
#include "SplitP.h"

#define GNC_PREFS_GROUP "dialogs.import.generic.transaction-list"
#define IMPORT_MAIN_MATCHER_CM_CLASS "transaction-matcher-dialog"

struct _main_matcher_info
{
    GtkWidget *main_widget;
    GtkTreeView *view;
    GNCImportSettings *user_settings;
    int selected_row;
    gboolean dark_theme;
    GNCTransactionProcessedCB transaction_processed_cb;
    gpointer user_data;
    GNCImportPendingMatches *pending_matches;
    GtkTreeViewColumn *account_column;
    GtkWidget         *show_account_column;
    GtkWidget         *show_matched_info;
    GtkWidget         *reconcile_after_close;
    gboolean add_toggled;   // flag to indicate that add has been toggled to stop selection
    gint id;
    GSList* temp_trans_list; // Temporary list of imported transactions
    GHashTable* acct_id_hash;     // Hash table, per account, of list of transaction IDs.
    GSList* edited_accounts; // List of accounts currently edited.
};

enum downloaded_cols
{
    DOWNLOADED_COL_DATE_TXT = 0,
    DOWNLOADED_COL_DATE_INT64, // used only for sorting
    DOWNLOADED_COL_ACCOUNT,
    DOWNLOADED_COL_AMOUNT,
    DOWNLOADED_COL_AMOUNT_DOUBLE, // used only for sorting
    DOWNLOADED_COL_DESCRIPTION,
    DOWNLOADED_COL_MEMO,
    DOWNLOADED_COL_ACTION_ADD,
    DOWNLOADED_COL_ACTION_CLEAR,
    DOWNLOADED_COL_ACTION_UPDATE,
    DOWNLOADED_COL_ACTION_INFO,
    DOWNLOADED_COL_ACTION_PIXBUF,
    DOWNLOADED_COL_DATA,
    DOWNLOADED_COL_COLOR,
    DOWNLOADED_COL_ENABLE,
    NUM_DOWNLOADED_COLS
};

#define CSS_INT_REQUIRED_CLASS      "gnc-class-intervention-required"
#define CSS_INT_PROB_REQUIRED_CLASS "gnc-class-intervention-probably-required"
#define CSS_INT_NOT_REQUIRED_CLASS  "gnc-class-intervention-not-required"

/* Define log domain for extended debugging of matcher */
#define G_MOD_IMPORT_MATCHER "gnc.import.main-matcher"
/*static QofLogModule log_module = GNC_MOD_IMPORT;*/
static QofLogModule log_module = G_MOD_IMPORT_MATCHER;

void on_matcher_ok_clicked (GtkButton *button, GNCImportMainMatcher *info);
void on_matcher_cancel_clicked (GtkButton *button, gpointer user_data);
gboolean on_matcher_delete_event (GtkWidget *widget, GdkEvent *event, gpointer data);
void on_matcher_help_clicked (GtkButton *button, gpointer user_data);
void on_matcher_help_close_clicked (GtkButton *button, gpointer user_data);

static void gnc_gen_trans_list_create_matches (GNCImportMainMatcher *gui);

/* Local prototypes */
static void gnc_gen_trans_assign_transfer_account (
                    GtkTreeView *treeview,
                    gboolean *first,
                    gboolean is_selection,
                    GtkTreePath *path,
                    Account **new_acc,
                    GNCImportMainMatcher *info);
static void gnc_gen_trans_assign_transfer_account_to_selection_cb (
                    GtkMenuItem *menuitem,
                    GNCImportMainMatcher *info);
static void gnc_gen_trans_view_popup_menu (
                    GtkTreeView *treeview,
                    GdkEvent *event,
                    GNCImportMainMatcher *info);
static gboolean gnc_gen_trans_onButtonPressed_cb (
                    GtkTreeView *treeview,
                    GdkEvent *event,
                    GNCImportMainMatcher *info);
static gboolean gnc_gen_trans_onPopupMenu_cb (
                    GtkTreeView *treeview,
                    GNCImportMainMatcher *info);
static void refresh_model_row (
                    GNCImportMainMatcher *gui,
                    GtkTreeModel *model,
                    GtkTreeIter *iter,
                    GNCImportTransInfo *info);
/* end local prototypes */

static gboolean delete_hash (gpointer key, gpointer value, gpointer user_data)
{
    // Value is a hash table that needs to be destroyed. 
    g_hash_table_destroy (value);
    return TRUE;
}

static void
update_all_balances (GNCImportMainMatcher *info)
{
    for (GSList* iter = info->edited_accounts; iter; iter=iter->next)
    {
        gnc_account_set_defer_bal_computation(iter->data,FALSE);
        xaccAccountRecomputeBalance(iter->data);
    }
    g_slist_free (info->edited_accounts);
    info->edited_accounts = NULL;
}

static void
defer_bal_computation (GNCImportMainMatcher *info, Account* acc)
{
    if (!gnc_account_get_defer_bal_computation(acc))
    {
        gnc_account_set_defer_bal_computation (acc, TRUE);
        info->edited_accounts = g_slist_prepend (info->edited_accounts, acc);
    }
}

void gnc_gen_trans_list_delete (GNCImportMainMatcher *info)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;

    if (info == NULL)
        return;

    model = gtk_tree_view_get_model (info->view);
    if (gtk_tree_model_get_iter_first (model, &iter))
    {
        do
        {
            gtk_tree_model_get (model, &iter,
                                DOWNLOADED_COL_DATA, &trans_info,
                                -1);

            if (info->transaction_processed_cb)
            {
                info->transaction_processed_cb (trans_info, FALSE,
                                                info->user_data);
            }
        }
        while (gtk_tree_model_iter_next (model, &iter));
    }

    if (GTK_IS_DIALOG(info->main_widget))
    {
        gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->main_widget));
        gnc_import_Settings_delete (info->user_settings);
        gnc_unregister_gui_component (info->id);
        gtk_widget_destroy (GTK_WIDGET (info->main_widget));
    }
    else
        gnc_import_Settings_delete (info->user_settings);

    g_slist_free_full (info->temp_trans_list,(GDestroyNotify) gnc_import_TransInfo_delete);
    info->temp_trans_list = NULL;
    
    // We've deferred balance computations on many accounts. Let's do it now that we're done.
    update_all_balances (info);
    
    g_hash_table_foreach_remove (info->acct_id_hash, delete_hash, NULL);
    info->acct_id_hash = NULL;
    g_free (info);
}

gboolean gnc_gen_trans_list_empty(GNCImportMainMatcher *info)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;
    g_assert (info);
    model = gtk_tree_view_get_model (info->view);
    // Check that both the tree model and the temporary list are empty.
    return !gtk_tree_model_get_iter_first (model, &iter) && !info->temp_trans_list;
}

void gnc_gen_trans_list_show_all(GNCImportMainMatcher *info)
{
    gnc_gen_trans_list_create_matches (info);
    gtk_widget_show_all (GTK_WIDGET (info->main_widget));
}

void
on_matcher_ok_clicked (GtkButton *button, GNCImportMainMatcher *info)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;

    g_assert (info);

    /*   DEBUG ("Begin") */

    model = gtk_tree_view_get_model (info->view);
    if (!gtk_tree_model_get_iter_first (model, &iter))
    {
        // No transaction, we can just close the dialog.
        gnc_gen_trans_list_delete (info);
        return;
    }

    /* Don't run any queries and/or split sorts while processing the matcher
    results. */
    gnc_suspend_gui_refresh();
    do
    {
        gtk_tree_model_get (model, &iter,
                            DOWNLOADED_COL_DATA, &trans_info,
                            -1);

        // Note: if there's only 1 split (unbalanced) one will be created with the unbalanced account,
        // and for that account the defer balance will not be set. So things will be slow.

        if (gnc_import_process_trans_item (NULL, trans_info))
        {
            if (info->transaction_processed_cb)
            {
                info->transaction_processed_cb (trans_info, TRUE,
                                               info->user_data);
            }
        }
    }
    while (gtk_tree_model_iter_next (model, &iter));
    
    gnc_gen_trans_list_delete (info);

    /* Allow GUI refresh again. */
    gnc_resume_gui_refresh();

    /* DEBUG ("End") */
}

void
on_matcher_cancel_clicked (GtkButton *button, gpointer user_data)
{
    GNCImportMainMatcher *info = user_data;
    gnc_gen_trans_list_delete (info);
}

gboolean
on_matcher_delete_event (GtkWidget *widget, GdkEvent *event, gpointer data)
{
    GNCImportMainMatcher *info = data;
    gnc_gen_trans_list_delete (info);
    return FALSE;
}

void
on_matcher_help_close_clicked (GtkButton *button, gpointer user_data)
{
    GtkWidget *help_dialog = user_data;

    gtk_widget_destroy (help_dialog);
}

void
on_matcher_help_clicked (GtkButton *button, gpointer user_data)
{
    GNCImportMainMatcher *info = user_data;
    GtkBuilder *builder;
    GtkWidget *help_dialog, *box;
    gchar *int_required_class, *int_prob_required_class, *int_not_required_class;
    gchar *class_extension = NULL;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer2");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer3");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer4");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer5");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer1");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "matcher_help_dialog");

    if (info->dark_theme == TRUE)
        class_extension = "-dark";

    int_required_class = g_strconcat (CSS_INT_REQUIRED_CLASS, class_extension, NULL);
    int_prob_required_class = g_strconcat (CSS_INT_PROB_REQUIRED_CLASS, class_extension, NULL);
    int_not_required_class = g_strconcat (CSS_INT_NOT_REQUIRED_CLASS, class_extension, NULL);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "intervention_required_box"));
    gnc_widget_style_context_add_class (GTK_WIDGET(box), int_required_class);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "intervention_probably_required_box"));
    gnc_widget_style_context_add_class (GTK_WIDGET(box), int_prob_required_class);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "intervention_not_required_box"));
    gnc_widget_style_context_add_class (GTK_WIDGET(box), int_not_required_class);

    help_dialog = GTK_WIDGET(gtk_builder_get_object (builder, "matcher_help_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(help_dialog), GTK_WINDOW(info->main_widget));

    /* Connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, help_dialog);

    g_object_unref (G_OBJECT(builder));

    g_free (int_required_class);
    g_free (int_prob_required_class);
    g_free (int_not_required_class);

    gtk_widget_show (help_dialog);
}

static void
run_account_picker_dialog (GNCImportMainMatcher *info,
                           GtkTreeModel *model,
                           GtkTreeIter *iter,
                           GNCImportTransInfo *trans_info)
{
    Account *old_acc, *new_acc;
    gboolean ok_pressed;
    g_assert (trans_info);
    old_acc = gnc_import_TransInfo_get_destacc (trans_info);

    new_acc = gnc_import_select_account (
            info->main_widget,
             NULL,
             TRUE,
             _("Destination account for the auto-balance split."),
             xaccTransGetCurrency (gnc_import_TransInfo_get_trans (trans_info)),
             ACCT_TYPE_NONE,
             old_acc,
             &ok_pressed);
    if (ok_pressed)
    {
        gnc_import_TransInfo_set_destacc (trans_info, new_acc, TRUE);
        defer_bal_computation (info, new_acc);
    }
}

static void
run_match_dialog (GNCImportMainMatcher *info,
                  GNCImportTransInfo *trans_info)
{
    gnc_import_match_picker_run_and_close (info->main_widget,
                                           trans_info, info->pending_matches);
}

static void
gnc_gen_trans_add_toggled_cb (GtkCellRendererToggle *cell_renderer,
                              gchar                 *path,
                              GNCImportMainMatcher  *gui)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;

    ENTER("");
    model = gtk_tree_view_get_model (gui->view);
    if (!gtk_tree_model_get_iter_from_string (model, &iter, path))
        return;
    gtk_tree_model_get (model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);

    if (gnc_import_TransInfo_get_action (trans_info) == GNCImport_ADD &&
            gnc_import_Settings_get_action_skip_enabled (gui->user_settings) == TRUE)
    {
        gnc_import_TransInfo_set_action (trans_info, GNCImport_SKIP);
    }
    else
    {
        gnc_import_TransInfo_set_action (trans_info, GNCImport_ADD);
    }
    refresh_model_row (gui, model, &iter, trans_info);
    LEAVE("");
}

static void
gnc_gen_trans_clear_toggled_cb (GtkCellRendererToggle *cell_renderer,
                                gchar                 *path,
                                GNCImportMainMatcher  *gui)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;

    ENTER("");
    model = gtk_tree_view_get_model (gui->view);

    if (!gtk_tree_model_get_iter_from_string (model, &iter, path))
        return;
    gtk_tree_model_get (model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);

    if (gnc_import_TransInfo_get_action (trans_info) == GNCImport_CLEAR &&
            gnc_import_Settings_get_action_skip_enabled (gui->user_settings) == TRUE)
    {
        gnc_import_TransInfo_set_action (trans_info, GNCImport_SKIP);
    }
    else
    {
        gnc_import_TransInfo_set_action (trans_info, GNCImport_CLEAR);
    }
    refresh_model_row (gui, model, &iter, trans_info);
    LEAVE("");
}

static void
gnc_gen_trans_update_toggled_cb (GtkCellRendererToggle *cell_renderer,
                                 gchar                 *path,
                                 GNCImportMainMatcher  *gui)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;

    ENTER("");
    model = gtk_tree_view_get_model (gui->view);

    if (!gtk_tree_model_get_iter_from_string (model, &iter, path))
        return;
    gtk_tree_model_get (model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);

    if (gnc_import_TransInfo_get_action (trans_info) == GNCImport_UPDATE &&
            gnc_import_Settings_get_action_skip_enabled (gui->user_settings) == TRUE)
    {
        gnc_import_TransInfo_set_action (trans_info, GNCImport_SKIP);
    }
    else
    {
        gnc_import_TransInfo_set_action (trans_info, GNCImport_UPDATE);
    }
    refresh_model_row (gui, model, &iter, trans_info);
    LEAVE("");
}

static void
gnc_gen_trans_assign_transfer_account (GtkTreeView *treeview,
                                       gboolean *first,
                                       gboolean is_selection,
                                       GtkTreePath *path,
                                       Account **new_acc,
                                       GNCImportMainMatcher *info)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;
    Account *old_acc;
    gboolean ok_pressed;
    gchar *path_str = gtk_tree_path_to_string (path);

    ENTER("");
    DEBUG("first = %s", *first ? "true" : "false");
    DEBUG("is_selection = %s", is_selection ? "true" : "false");
    DEBUG("path  = %s", path_str);
    g_free (path_str);
    DEBUG("account passed in = %s", gnc_get_account_name_for_register (*new_acc));

    // only allow response at the top level
    if (gtk_tree_path_get_depth (path) != 1)
        return;

    model = gtk_tree_view_get_model (treeview);
    if (gtk_tree_model_get_iter (model, &iter, path))
    {
        gtk_tree_model_get (model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);

        switch (gnc_import_TransInfo_get_action (trans_info))
        {
        case GNCImport_ADD:
            if (gnc_import_TransInfo_is_balanced (trans_info) == FALSE)
            {
                ok_pressed = TRUE;
                old_acc  = gnc_import_TransInfo_get_destacc (trans_info);
                if (*first)
                {
                    ok_pressed = FALSE;
                    *new_acc = gnc_import_select_account (info->main_widget,
                        NULL,
                        TRUE,
                        _("Destination account for the auto-balance split."),
                        xaccTransGetCurrency (
                              gnc_import_TransInfo_get_trans (trans_info)),
                        ACCT_TYPE_NONE,
                        old_acc,
                        &ok_pressed);
                    *first = FALSE;
                    DEBUG("account selected = %s",
                            gnc_account_get_full_name (*new_acc));
                }
                if (ok_pressed)
                {
                    gnc_import_TransInfo_set_destacc (trans_info, *new_acc, TRUE);
                    defer_bal_computation (info, *new_acc);
                }
            }
            break;
        case GNCImport_CLEAR:
        case GNCImport_UPDATE:
            if (*first && !is_selection)
                run_match_dialog (info, trans_info);
            break;
        case GNCImport_SKIP:
            break;
        default:
            PERR("InvalidGNCImportValue");
            break;
        }
        refresh_model_row (info, model, &iter, trans_info);
    }
    LEAVE("");
}

static void
gnc_gen_trans_assign_transfer_account_to_selection_cb (
            GtkMenuItem *menuitem,
            GNCImportMainMatcher *info)
{
    GtkTreeView *treeview;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;
    Account *assigned_account;
    GList *selected_rows, *l;
    gboolean first, is_selection;
    GList *refs = NULL;

    ENTER("assign_transfer_account_to_selection_cb");
    treeview = GTK_TREE_VIEW(info->view);
    model = gtk_tree_view_get_model (treeview);
    selection = gtk_tree_view_get_selection (treeview);
    selected_rows = gtk_tree_selection_get_selected_rows (selection, &model);
    assigned_account = NULL;
    first = TRUE;
    is_selection = TRUE;

    DEBUG("Rows in selection = %i",
          gtk_tree_selection_count_selected_rows (selection));
    DEBUG("Entering loop over selection");

    if (gtk_tree_selection_count_selected_rows (selection) > 0)
    {
        for (l = selected_rows; l != NULL; l = l->next)
        {
            gchar *path_str = gtk_tree_path_to_string (l->data);
            GtkTreeRowReference *ref = gtk_tree_row_reference_new (model, l->data);
            DEBUG("passing first = %s", first ? "true" : "false");
            DEBUG("passing is_selection = %s",
                        is_selection ? "true" : "false");
            DEBUG("passing path = %s", path_str);
            g_free (path_str);
            refs = g_list_prepend (refs, ref);
            DEBUG("passing account value = %s",
                        gnc_account_get_full_name (assigned_account));
            gnc_gen_trans_assign_transfer_account (treeview,
                        &first, is_selection, l->data,
                        &assigned_account, info);
            DEBUG("returned value of account = %s",
                        gnc_account_get_full_name (assigned_account));
            DEBUG("returned value of first = %s", first ? "true" : "false");
            if (assigned_account == NULL)
                break;

        }
    }
    g_list_free_full (selected_rows, (GDestroyNotify) gtk_tree_path_free);

    // now reselect the transaction rows. This is very slow if there are lots of transactions.
    for (l = refs; l != NULL; l = l->next)
    {
        GtkTreePath *path = gtk_tree_row_reference_get_path (l->data);

        gtk_tree_selection_select_path (selection, path);

        gtk_tree_path_free (path);
        gtk_tree_row_reference_free (l->data);
    }
    g_list_free (refs);

    LEAVE("");
}

static void
gnc_gen_trans_row_activated_cb (GtkTreeView *treeview,
                                GtkTreePath *path,
                                GtkTreeViewColumn *column,
                                GNCImportMainMatcher *info)
{
    Account *assigned_account;
    gboolean first, is_selection;

    ENTER("");
    assigned_account = NULL;
    first = TRUE;
    is_selection = FALSE;
    gnc_gen_trans_assign_transfer_account (treeview,
                            &first, is_selection, path,
                            &assigned_account, info);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (treeview), path);

    DEBUG("account returned = %s", gnc_account_get_full_name (assigned_account));
    LEAVE("");
}

static GNCImportAction
get_action_for_path (GtkTreePath* path, GtkTreeModel *model)
{
    GNCImportTransInfo *trans_info;
    GtkTreeIter iter;
    gtk_tree_model_get_iter (model, &iter, path);
    gtk_tree_model_get (model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);
    return gnc_import_TransInfo_get_action (trans_info);
}

static void
gnc_gen_trans_row_changed_cb (GtkTreeSelection *selection,
                              GNCImportMainMatcher *info)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GtkSelectionMode mode;

    ENTER("");
    mode = gtk_tree_selection_get_mode (selection);
    if (gtk_tree_selection_count_selected_rows (selection) >= 2)
    {
        // Unselect rows that should not be selectable
        GList* list = gtk_tree_selection_get_selected_rows (selection, &model);
        for ( ; list; list=list->next)
        {
            if (get_action_for_path (list->data, model) != GNCImport_ADD)
                gtk_tree_selection_unselect_path (selection, list->data);
        }
        g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);
    }
    
    switch (mode)
    {
        case GTK_SELECTION_MULTIPLE:
            DEBUG("mode = GTK_SELECTION_MULTIPLE, no action");
            break;
        case GTK_SELECTION_NONE:
            DEBUG("mode = GTK_SELECTION_NONE, no action");
            break;
        case GTK_SELECTION_BROWSE:
            DEBUG("mode = GTK_SELECTION_BROWSE->default");
        case GTK_SELECTION_SINGLE:
            DEBUG("mode = GTK_SELECTION_SINGLE->default");
        default:
            DEBUG("mode = default unselect selected row");
            if (gtk_tree_selection_get_selected (selection, &model, &iter))
            {
                gtk_tree_selection_unselect_iter (selection, &iter);
            }
    }
    LEAVE("");
}

static void
gnc_gen_trans_view_popup_menu (GtkTreeView *treeview,
                               GdkEvent *event,
                               GNCImportMainMatcher *info)
{
    GtkWidget *menu, *menuitem;
    GdkEventButton *event_button;

    ENTER ("");
    menu = gtk_menu_new();
    menuitem = gtk_menu_item_new_with_label (
            _("Assign a transfer account to the selection."));
    g_signal_connect (menuitem, "activate",
                      G_CALLBACK(
                      gnc_gen_trans_assign_transfer_account_to_selection_cb),
                      info);
    DEBUG("Callback to assign destination account to selection connected");
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), menuitem);
    gtk_widget_show_all (menu);
    event_button = (GdkEventButton *) event;
    /* Note: event can be NULL here when called from view_onPopupMenu; */
    gtk_menu_popup_at_pointer (GTK_MENU(menu), (GdkEvent*)event);

    LEAVE ("");
}

static gboolean
gnc_gen_trans_onButtonPressed_cb (GtkTreeView *treeview,
                                  GdkEvent *event,
                                  GNCImportMainMatcher *info)
{
    GdkEventButton *event_button;
    GtkTreeSelection *selection;
    ENTER("");
    g_return_val_if_fail (treeview != NULL, FALSE);
    g_return_val_if_fail (event != NULL, FALSE);
    /* handle single click with the right mouse button? */
    if (event->type == GDK_BUTTON_PRESS)
    {
        event_button = (GdkEventButton *) event;
        if (event_button->button == GDK_BUTTON_SECONDARY)
        {
            int count = 0;
            DEBUG("Right mouseClick detected- popup the menu.");
            // Only pop up the menu if there's more than 1 selected transaction,
            // or the selected transaction is an ADD.
            selection = gtk_tree_view_get_selection (treeview);
            count = gtk_tree_selection_count_selected_rows (selection);
            if (count > 1)
                gnc_gen_trans_view_popup_menu (treeview, event, info);
            else if (count > 0)
            {
                GList* selected;
                GtkTreeModel *model;
                selected = gtk_tree_selection_get_selected_rows (selection, &model);
                get_action_for_path (selected->data, model);
                if (get_action_for_path (selected->data, model) == GNCImport_ADD)
                    gnc_gen_trans_view_popup_menu (treeview, event, info);
                g_list_free_full (selected, (GDestroyNotify) gtk_tree_path_free);
            }
            LEAVE("return TRUE");
            return TRUE;
        }
    }
    LEAVE("return FALSE");
    return FALSE;
}

static gboolean
gnc_gen_trans_onPopupMenu_cb (GtkTreeView *treeview,
                              GNCImportMainMatcher *info)
{
    GtkTreeSelection *selection;
    ENTER("onPopupMenu_cb");
    /* respond to Shift-F10 popup menu hotkey */
    selection = gtk_tree_view_get_selection (treeview);
    if (gtk_tree_selection_count_selected_rows (selection) > 0)
    {
      gnc_gen_trans_view_popup_menu (treeview, NULL, info);
      LEAVE ("TRUE");
      return TRUE;
    }
    LEAVE ("FALSE");
    return TRUE;
}

static GtkTreeViewColumn *
add_text_column(GtkTreeView *view, const gchar *title, int col_num, gboolean ellipsize)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes
             (title, renderer,
              "text", col_num,
              "background", DOWNLOADED_COL_COLOR,
              NULL);

    if (ellipsize)
        g_object_set (renderer, "ellipsize", PANGO_ELLIPSIZE_END, NULL);

    // If date column, use the time64 value for the sorting.
    if (col_num == DOWNLOADED_COL_DATE_TXT)
        gtk_tree_view_column_set_sort_column_id(column, DOWNLOADED_COL_DATE_INT64);
    else if (col_num == DOWNLOADED_COL_AMOUNT) // If amount column, use double value
    {
        gtk_cell_renderer_set_alignment (renderer, 1.0, 0.5); // right align amount column
        gtk_cell_renderer_set_padding (renderer, 5, 0); // add padding so its not close to description
        gtk_tree_view_column_set_sort_column_id (column, DOWNLOADED_COL_AMOUNT_DOUBLE);
    }
    else
        gtk_tree_view_column_set_sort_column_id (column, col_num);

    g_object_set (G_OBJECT(column),
                  "reorderable", TRUE,
                  "resizable", TRUE,
                  NULL);
    gtk_tree_view_append_column (view, column);
    return column;
}

static GtkTreeViewColumn *
add_toggle_column (GtkTreeView *view, const gchar *title, int col_num,
                   GCallback cb_fn, gpointer cb_arg)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    renderer = gtk_cell_renderer_toggle_new();
    column = gtk_tree_view_column_new_with_attributes
             (title, renderer,
              "active", col_num,
              "cell-background", DOWNLOADED_COL_COLOR,
              "activatable", DOWNLOADED_COL_ENABLE,
              "visible", DOWNLOADED_COL_ENABLE,
              NULL);
    gtk_tree_view_column_set_sort_column_id (column, col_num);
    g_object_set (G_OBJECT(column),
                  "reorderable", TRUE,
                  NULL);
    g_signal_connect (renderer, "toggled", cb_fn, cb_arg);
    gtk_tree_view_append_column (view, column);
    return column;
}

static void
gnc_gen_trans_init_view (GNCImportMainMatcher *info,
                         gboolean show_account,
                         gboolean show_update)
{
    GtkTreeView *view;
    GtkTreeStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;

    view = info->view;
    store = gtk_tree_store_new (NUM_DOWNLOADED_COLS, G_TYPE_STRING, G_TYPE_INT64,
                                G_TYPE_STRING, G_TYPE_STRING, G_TYPE_DOUBLE,
                                G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN,
                                G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_STRING,
                                GDK_TYPE_PIXBUF, G_TYPE_POINTER, G_TYPE_STRING,
                                G_TYPE_BOOLEAN);
    gtk_tree_view_set_model (view, GTK_TREE_MODEL(store));
    g_object_unref (store);

    /* prevent the rows being dragged to a different order */
    gtk_tree_view_set_reorderable (view, FALSE);

    /* Add the columns */
    add_text_column (view, _("Date"), DOWNLOADED_COL_DATE_TXT, FALSE);
    info->account_column = add_text_column (view, _("Account"), DOWNLOADED_COL_ACCOUNT, FALSE);
    gtk_tree_view_column_set_visible (info->account_column, show_account);
    add_text_column (view, _("Amount"), DOWNLOADED_COL_AMOUNT, FALSE);
    add_text_column (view, _("Description"), DOWNLOADED_COL_DESCRIPTION, FALSE);
    add_text_column (view, _("Memo"), DOWNLOADED_COL_MEMO, TRUE);
    add_toggle_column (view,
                       C_("Column header for 'Adding transaction'", "A"), DOWNLOADED_COL_ACTION_ADD,
                       G_CALLBACK(gnc_gen_trans_add_toggled_cb), info);
    column = add_toggle_column (view,
                               C_("Column header for 'Updating plus Clearing transaction'", "U+C"), DOWNLOADED_COL_ACTION_UPDATE,
                               G_CALLBACK(gnc_gen_trans_update_toggled_cb), info);
    gtk_tree_view_column_set_visible (column, show_update);
    add_toggle_column (view,
                       C_("Column header for 'Clearing transaction'", "C"), DOWNLOADED_COL_ACTION_CLEAR,
                      G_CALLBACK(gnc_gen_trans_clear_toggled_cb), info);

    /* The last column has multiple renderers */
    renderer = gtk_cell_renderer_pixbuf_new();
    g_object_set (renderer, "xalign", 0.0, NULL);
    column = gtk_tree_view_column_new_with_attributes (_("Info"), renderer,
             "pixbuf", DOWNLOADED_COL_ACTION_PIXBUF,
             "cell-background", DOWNLOADED_COL_COLOR,
             NULL);

    gtk_tree_view_append_column (info->view, column);

    column = add_text_column (view, _("Additional Comments"), DOWNLOADED_COL_ACTION_INFO, FALSE);
    gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);

    /* default sort order */
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(store),
                                          DOWNLOADED_COL_DATE_INT64,
                                          GTK_SORT_ASCENDING);
    selection = gtk_tree_view_get_selection (info->view);

    g_signal_connect (info->view, "row-activated",
                      G_CALLBACK(gnc_gen_trans_row_activated_cb), info);
    g_signal_connect (selection, "changed",
                      G_CALLBACK(gnc_gen_trans_row_changed_cb), info);

    g_signal_connect (view, "button-press-event",
                      G_CALLBACK(gnc_gen_trans_onButtonPressed_cb), info);
    g_signal_connect (view, "popup-menu",
                      G_CALLBACK(gnc_gen_trans_onPopupMenu_cb), info);
    
    info->acct_id_hash = g_hash_table_new (g_direct_hash, g_direct_equal);
}

static void
show_account_column_toggled_cb (GtkToggleButton *togglebutton,
                                GNCImportMainMatcher *info)
{
    gtk_tree_view_column_set_visible (info->account_column,
        gtk_toggle_button_get_active (togglebutton));
}

static void
show_matched_info_toggled_cb (GtkToggleButton *togglebutton,
                                GNCImportMainMatcher *info)
{
    if (gtk_toggle_button_get_active (togglebutton))
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->show_account_column), TRUE);
        gtk_tree_view_expand_all (info->view);
    }
    else
    {
        gtk_tree_view_column_set_visible (info->account_column,
            gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->show_account_column)));
        gtk_tree_view_collapse_all (info->view);
    }
}

GNCImportMainMatcher *gnc_gen_trans_list_new (GtkWidget *parent,
        const gchar* heading,
        gboolean all_from_same_account,
        gint match_date_hardlimit,
        gboolean show_all)
{
    GNCImportMainMatcher *info;
    GtkBuilder *builder;
    GtkWidget *heading_label;
    GtkWidget *box, *pbox;
    gboolean show_update;
    GtkStyleContext *stylectxt;
    GdkRGBA color;
    GtkWidget *button;

    info = g_new0 (GNCImportMainMatcher, 1);
    info->pending_matches = gnc_import_PendingMatches_new();

    /* Initialize user Settings. */
    info->user_settings = gnc_import_Settings_new ();
    gnc_import_Settings_set_match_date_hardlimit (info->user_settings, match_date_hardlimit);

    stylectxt = gtk_widget_get_style_context (GTK_WIDGET(parent));
    gtk_style_context_get_color (stylectxt, GTK_STATE_FLAG_NORMAL, &color);
    info->dark_theme = gnc_is_dark_theme (&color);

    /* Initialize the GtkDialog. */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_dialog");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_content");
    info->main_widget = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_dialog"));
    g_assert (info->main_widget != NULL);

    /* Pack the content into the dialog vbox */
    pbox = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_vbox"));
    box = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_content"));
    gtk_box_pack_start (GTK_BOX(pbox), box, TRUE, TRUE, 0);

    /* Get the view */
    info->view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "downloaded_view"));
    g_assert (info->view != NULL);

    info->show_account_column = GTK_WIDGET(gtk_builder_get_object (builder, "show_source_account_button"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->show_account_column), all_from_same_account);
    g_signal_connect (G_OBJECT(info->show_account_column), "toggled",
                      G_CALLBACK(show_account_column_toggled_cb), info);

    info->show_matched_info = GTK_WIDGET(gtk_builder_get_object (builder, "show_matched_info_button"));
    g_signal_connect (G_OBJECT(info->show_matched_info), "toggled",
                      G_CALLBACK(show_matched_info_toggled_cb), info);

    // Create the checkbox, but do not show it by default.
    info->reconcile_after_close = GTK_WIDGET(gtk_builder_get_object (builder, "reconcile_after_close_button"));

    show_update = gnc_import_Settings_get_action_update_enabled (info->user_settings);
    gnc_gen_trans_init_view (info, all_from_same_account, show_update);
    heading_label = GTK_WIDGET(gtk_builder_get_object (builder, "heading_label"));
    g_assert (heading_label != NULL);

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (info->main_widget), GTK_WINDOW (parent));

    if (heading)
        gtk_label_set_text (GTK_LABEL (heading_label), heading);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->main_widget), GTK_WINDOW (parent));
    if(show_all)
        gtk_widget_show_all (GTK_WIDGET (info->main_widget));

    info->transaction_processed_cb = NULL;

    /* Connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, info);

    g_object_unref (G_OBJECT(builder));

    // Register this UI, it needs to be closed when the session is closed.
    info->id = gnc_register_gui_component (IMPORT_MAIN_MATCHER_CM_CLASS,
                                           NULL, /* no refresh handler */
                                           (GNCComponentCloseHandler)gnc_gen_trans_list_delete,
                                           info);
    // This ensure this dialog is closed when the session is closed.
    gnc_gui_component_set_session (info->id, gnc_get_current_session());
    return info;
}

/*****************************************************************
 *                 Assistant routines Start                      *
 *****************************************************************/

GNCImportMainMatcher * gnc_gen_trans_assist_new (GtkWidget *parent,
        GtkWidget *assistant_page, const gchar* heading,
        gboolean all_from_same_account, gint match_date_hardlimit)
{
    GNCImportMainMatcher *info;
    GtkBuilder *builder;
    GtkWidget *heading_label;
    GtkWidget *box;
    gboolean show_update;
    GtkStyleContext *stylectxt;
    GdkRGBA color;

    info = g_new0 (GNCImportMainMatcher, 1);
    info->pending_matches = gnc_import_PendingMatches_new();
    info->main_widget = GTK_WIDGET(parent);

    /* Initialize user Settings. */
    info->user_settings = gnc_import_Settings_new ();
    gnc_import_Settings_set_match_date_hardlimit (info->user_settings, match_date_hardlimit);

    stylectxt = gtk_widget_get_style_context (GTK_WIDGET(parent));
    gtk_style_context_get_color (stylectxt, GTK_STATE_FLAG_NORMAL, &color);
    info->dark_theme = gnc_is_dark_theme (&color);

    /* load the interface */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_content");
    if (builder == NULL)
    {
        PERR("Error opening the glade builder interface");
    }
    /* Pack content into Assistant page widget */
    box = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_content"));
    gtk_box_pack_start (GTK_BOX(assistant_page), box, TRUE, TRUE, 6);

    /* Get the view */
    info->view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "downloaded_view"));
    g_assert (info->view != NULL);

    info->show_account_column = GTK_WIDGET(gtk_builder_get_object (builder, "show_source_account_button"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->show_account_column), all_from_same_account);
    g_signal_connect (G_OBJECT(info->show_account_column), "toggled",
                      G_CALLBACK(show_account_column_toggled_cb), info);

    info->show_matched_info = GTK_WIDGET(gtk_builder_get_object (builder, "show_matched_info_button"));
    g_signal_connect (G_OBJECT(info->show_matched_info), "toggled",
                      G_CALLBACK(show_matched_info_toggled_cb), info);

    // Create the checkbox, but do not show it by default.
    info->reconcile_after_close = GTK_WIDGET(gtk_builder_get_object (builder, "reconcile_after_close_button"));

    show_update = gnc_import_Settings_get_action_update_enabled (info->user_settings);
    gnc_gen_trans_init_view (info, all_from_same_account, show_update);
    heading_label = GTK_WIDGET(gtk_builder_get_object (builder, "heading_label"));
    g_assert (heading_label != NULL);

    if (heading)
        gtk_label_set_text (GTK_LABEL (heading_label), heading);

    info->transaction_processed_cb = NULL;

    /* Connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, info);

    g_object_unref (G_OBJECT(builder));

    return info;
}

void gnc_gen_trans_assist_start (GNCImportMainMatcher *info)
{
    on_matcher_ok_clicked (NULL, info);
}

/*****************************************************************
 *                   Assistant routines End                      *
 *****************************************************************/

void gnc_gen_trans_list_add_tp_cb (GNCImportMainMatcher *info,
                                   GNCTransactionProcessedCB trans_processed_cb,
                                   gpointer user_data)
{
    info->user_data = user_data;
    info->transaction_processed_cb = trans_processed_cb;
}

gboolean gnc_gen_trans_list_run (GNCImportMainMatcher *info)
{
    gboolean result;

    /* DEBUG("Begin"); */
    result = gtk_dialog_run (GTK_DIALOG (info->main_widget));
    /* DEBUG("Result was %d", result); */

    /* No destroying here since the dialog was already destroyed through
       the ok_clicked handlers. */

    return result;
}

static gchar*
get_required_color (const gchar *class_name)
{
    GdkRGBA color;
    GtkWidget *label = gtk_label_new ("Color");
    GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET(label));
    gtk_style_context_add_class (context, class_name);
    gnc_style_context_get_background_color (context, GTK_STATE_FLAG_NORMAL, &color);
    return gdk_rgba_to_string (&color);
}

static void
remove_child_row (GtkTreeModel *model, GtkTreeIter *iter)
{
    if (gtk_tree_model_iter_has_child (model, iter))
    {
        GtkTreeIter  child;
        gtk_tree_model_iter_nth_child (model, &child, iter, 0);
        gtk_tree_store_remove (GTK_TREE_STORE(model), &child);
    }
}

static void
update_child_row (GNCImportMatchInfo *sel_match, GtkTreeModel *model, GtkTreeIter *iter)
{
    GtkTreeStore *store;
    GtkTreeIter  child;
    gchar *text = qof_print_date (xaccTransGetDate (sel_match->trans));
    const gchar *ro_text;

    const gchar *desc = xaccTransGetDescription (sel_match->trans);
    const gchar *memo = xaccSplitGetMemo (sel_match->split);

    store = GTK_TREE_STORE(model);

    if (!gtk_tree_model_iter_has_child (model, iter))
        gtk_tree_store_append (GTK_TREE_STORE(model), &child, iter);
    else
        gtk_tree_model_iter_nth_child (model, &child, iter, 0);

    gtk_tree_store_set (store, &child, DOWNLOADED_COL_DATE_TXT, text, -1);

    if (xaccTransCountSplits(sel_match->trans) == 2)
        gtk_tree_store_set (store, &child, DOWNLOADED_COL_ACCOUNT, xaccAccountGetName (
                            xaccSplitGetAccount (xaccSplitGetOtherSplit (sel_match->split))), -1);
    else
        gtk_tree_store_set (store, &child, DOWNLOADED_COL_ACCOUNT, _("-- Split Transaction --"), -1);

    ro_text = xaccPrintAmount (xaccSplitGetAmount (sel_match->split),
                               gnc_split_amount_print_info (sel_match->split, TRUE));

    gtk_tree_store_set (store, &child, DOWNLOADED_COL_AMOUNT, ro_text, -1);
    gtk_tree_store_set (store, &child, DOWNLOADED_COL_MEMO, memo, -1);
    gtk_tree_store_set (store, &child, DOWNLOADED_COL_DESCRIPTION, desc, -1);

    gtk_tree_store_set (store, &child, DOWNLOADED_COL_ENABLE, FALSE, -1);
    g_free (text);
}

static void
refresh_model_row (GNCImportMainMatcher *gui,
                   GtkTreeModel *model,
                   GtkTreeIter *iter,
                   GNCImportTransInfo *info)
{
    GtkTreeStore *store;
    GtkTreeSelection *selection;
    gchar *tmp, *imbalance, *text, *color;
    const gchar *ro_text;
    gchar *int_required_class, *int_prob_required_class, *int_not_required_class;
    gchar *class_extension = NULL;
    gboolean show_pixbuf = TRUE;
    Split *split;
    time64 date;
    gnc_numeric amount;
    g_assert (gui);
    g_assert (model);
    g_assert (info);
    /*DEBUG("Begin");*/

    store = GTK_TREE_STORE(model);
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_DATA, info, -1);

    if (gui->dark_theme == TRUE)
        class_extension = "-dark";

    int_required_class = g_strconcat (CSS_INT_REQUIRED_CLASS, class_extension, NULL);
    int_prob_required_class = g_strconcat (CSS_INT_PROB_REQUIRED_CLASS, class_extension, NULL);
    int_not_required_class = g_strconcat (CSS_INT_NOT_REQUIRED_CLASS, class_extension, NULL);

    /* This controls the visibility of the toggle cells */
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_ENABLE, TRUE, -1);

    /*Account:*/
    split = gnc_import_TransInfo_get_fsplit (info);
    g_assert (split); // Must not be NULL
    ro_text = xaccAccountGetName (xaccSplitGetAccount (split));
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_ACCOUNT, ro_text, -1);

    /*Date*/
    date = xaccTransGetDate (gnc_import_TransInfo_get_trans (info));
    text = qof_print_date (date);
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_DATE_TXT, text, -1);
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_DATE_INT64, date, -1);
    g_free(text);

    /*Amount*/
    amount = xaccSplitGetAmount (split);
    ro_text = xaccPrintAmount (amount, gnc_split_amount_print_info (split, TRUE));
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_AMOUNT, ro_text, -1);
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_AMOUNT_DOUBLE, gnc_numeric_to_double (amount), -1);

    /*Description*/
    ro_text = xaccTransGetDescription (gnc_import_TransInfo_get_trans (info) );
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_DESCRIPTION, ro_text, -1);

    /*Memo*/
    ro_text = xaccSplitGetMemo (split);
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_MEMO, ro_text, -1);

    /*Actions*/

    /* Action information */
    ro_text = text = color = NULL;
    switch (gnc_import_TransInfo_get_action (info))
    {
    case GNCImport_ADD:
        if (gnc_import_TransInfo_is_balanced (info) == TRUE)
        {
            ro_text = _("New, already balanced");
            color = get_required_color (int_not_required_class);
        }
        else
        {
            /* Assume that importers won't create transactions in two or more
               currencies so we can use xaccTransGetImbalanceValue */
            imbalance =
                g_strdup
                (xaccPrintAmount
                 (gnc_numeric_neg (xaccTransGetImbalanceValue
                                  (gnc_import_TransInfo_get_trans (info))),
                  gnc_commodity_print_info
                  (xaccTransGetCurrency (gnc_import_TransInfo_get_trans (info)),
                   TRUE)));
            if (gnc_import_TransInfo_get_destacc (info) != NULL)
            {
                color = get_required_color (int_not_required_class);
                tmp = gnc_account_get_full_name
                      (gnc_import_TransInfo_get_destacc (info));
                if (gnc_import_TransInfo_get_destacc_selected_manually (info)
                        == TRUE)
                {
                    text =
                        /* Translators: %1$s is the amount to be
                           transferred. %2$s is the destination account. */
                        g_strdup_printf (_("New, transfer %s to (manual) \"%s\""),
                                         imbalance, tmp);
                }
                else
                {
                    text =
                        /* Translators: %1$s is the amount to be
                           transferred. %2$s is the destination account. */
                        g_strdup_printf (_("New, transfer %s to (auto) \"%s\""),
                                         imbalance, tmp);
                }
                g_free (tmp);

            }
            else
            {
                color = get_required_color (int_prob_required_class);
                text =
                    /* Translators: %s is the amount to be transferred. */
                    g_strdup_printf (_("New, UNBALANCED (need acct to transfer %s)!"),
                                     imbalance);
            }
            remove_child_row (model, iter);

            g_free (imbalance);
        }
        break;
    case GNCImport_CLEAR:
        {
            GNCImportMatchInfo *sel_match = gnc_import_TransInfo_get_selected_match (info);

            if (sel_match)
            {
                color = get_required_color (int_not_required_class);
                if (gnc_import_TransInfo_get_match_selected_manually (info))
                {
                    ro_text = _("Reconcile (manual) match");
                }
                else
                {
                    ro_text = _("Reconcile (auto) match");
                }
                update_child_row (sel_match, model, iter);
            }
            else
            {
                color = get_required_color (int_required_class);
                ro_text = _("Match missing!");
                show_pixbuf = FALSE;
                remove_child_row (model, iter);
            }
        }
        break;
    case GNCImport_UPDATE:
        {
            GNCImportMatchInfo *sel_match = gnc_import_TransInfo_get_selected_match (info);

            if (sel_match)
            {
                color = get_required_color (int_not_required_class);
                if (gnc_import_TransInfo_get_match_selected_manually (info))
                {
                    ro_text = _("Update and reconcile (manual) match");
                }
                else
                {
                    ro_text = _("Update and reconcile (auto) match");
                }
                update_child_row (sel_match, model, iter);
            }
            else
            {
                color = get_required_color (int_required_class);
                ro_text = _("Match missing!");
                show_pixbuf = FALSE;
                remove_child_row (model, iter);
            }
        }
        break;
    case GNCImport_SKIP:
        color = get_required_color (int_required_class);
        ro_text = _("Do not import (no action selected)");
        show_pixbuf = FALSE;
        remove_child_row (model, iter);
        break;
    default:
        color = "white";
        ro_text = "WRITEME, this is an unknown action";
        show_pixbuf = FALSE;
        break;
    }

    gtk_tree_store_set (store, iter,
                        DOWNLOADED_COL_COLOR, color,
                        DOWNLOADED_COL_ACTION_INFO, ro_text ? ro_text : text,
                        -1);
    if (text)
        g_free (text);

    g_free (int_required_class);
    g_free (int_prob_required_class);
    g_free (int_not_required_class);

    /* Set the pixmaps */
    gtk_tree_store_set (store, iter,
                        DOWNLOADED_COL_ACTION_ADD,
                        gnc_import_TransInfo_get_action (info) == GNCImport_ADD,
                        -1);
    if (gnc_import_TransInfo_get_action (info) == GNCImport_SKIP)
    {
        /*If skipping the row, there is no best match's confidence pixmap*/
        gtk_tree_store_set (store, iter, DOWNLOADED_COL_ACTION_PIXBUF, NULL, -1);
    }

    gtk_tree_store_set (store, iter,
                        DOWNLOADED_COL_ACTION_CLEAR,
                        gnc_import_TransInfo_get_action (info) == GNCImport_CLEAR,
                        -1);
    if (gnc_import_TransInfo_get_action (info) == GNCImport_CLEAR)
    {
        /*Show the best match's confidence pixmap in the info column*/
        if (show_pixbuf)
            gtk_tree_store_set (store, iter,
                                DOWNLOADED_COL_ACTION_PIXBUF,
                                gen_probability_pixbuf (gnc_import_MatchInfo_get_probability
                                        (gnc_import_TransInfo_get_selected_match (info)),
                                        gui->user_settings,
                                        GTK_WIDGET(gui->view)),
                                -1);
        else
            gtk_tree_store_set (store, iter, DOWNLOADED_COL_ACTION_PIXBUF, NULL, -1);
    }

    gtk_tree_store_set (store, iter,
                        DOWNLOADED_COL_ACTION_UPDATE,
                        gnc_import_TransInfo_get_action (info) == GNCImport_UPDATE,
                        -1);
    if (gnc_import_TransInfo_get_action (info) == GNCImport_UPDATE)
    {
        /*Show the best match's confidence pixmap in the info column*/
        if (show_pixbuf)
            gtk_tree_store_set (store, iter,
                                DOWNLOADED_COL_ACTION_PIXBUF,
                                gen_probability_pixbuf (gnc_import_MatchInfo_get_probability
                                        (gnc_import_TransInfo_get_selected_match (info)),
                                        gui->user_settings,
                                        GTK_WIDGET(gui->view)),
                                -1);
        else
            gtk_tree_store_set (store, iter, DOWNLOADED_COL_ACTION_PIXBUF, NULL, -1);
    }

    // show child row if 'show matched info' is toggled
    if (gtk_tree_model_iter_has_child (model, iter))
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(gui->show_matched_info)))
        {
            GtkTreePath *path = gtk_tree_model_get_path (model, iter);

            gtk_tree_view_column_set_visible (gui->account_column, TRUE);

            gtk_tree_view_expand_row (GTK_TREE_VIEW(gui->view), path, TRUE);
            gtk_tree_path_free (path);
        }
    }
    selection = gtk_tree_view_get_selection (gui->view);
    gtk_tree_selection_unselect_all (selection);
}

void gnc_gen_trans_list_add_trans (GNCImportMainMatcher *gui, Transaction *trans)
{
    Account* acc = NULL;
    Split* split = NULL;
    int i=0;
    
    split = xaccTransGetSplit (trans, 0);
    acc = xaccSplitGetAccount (split);
    defer_bal_computation (gui, acc);
    
    gnc_gen_trans_list_add_trans_with_ref_id (gui, trans, 0);
    return;
}/* end gnc_import_add_trans() */

void
gnc_gen_trans_list_show_reconcile_after_close_button (GNCImportMainMatcher *info,
                                                      gboolean reconcile_after_close,
                                                      gboolean active)
{
    gtk_widget_set_visible (info->reconcile_after_close, reconcile_after_close);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (info->reconcile_after_close), active);
}

GtkWidget*
gnc_gen_trans_list_get_reconcile_after_close_button (GNCImportMainMatcher *info)
{
    return info->reconcile_after_close;
}

void gnc_gen_trans_list_add_trans_with_ref_id (GNCImportMainMatcher *gui, Transaction *trans, guint32 ref_id)
{
    GNCImportTransInfo * transaction_info = NULL;
    GtkTreeModel *model;
    GtkTreeIter iter;
    g_assert (gui);
    g_assert (trans);
  
    if (gnc_import_exists_online_id (trans, gui->acct_id_hash))
        return;
    else
    {
        transaction_info = gnc_import_TransInfo_new (trans, NULL);
        gnc_import_TransInfo_set_ref_id (transaction_info, ref_id);
        // It's much faster to gather the imported transactions into a GSList than directly into the
        // treeview.
        gui->temp_trans_list = g_slist_prepend (gui->temp_trans_list, transaction_info);
    }
    return;
}

// This creates lists of all splits that could match one of the imported transactions based on their
// account and date and organized per account
static GHashTable*
create_list_of_potential_matches (GNCImportMainMatcher *gui, GtkTreeModel* model, Query *query, gint match_date_hardlimit)
{
    GList* query_return = NULL;
    Account *import_trans_account;
    time64 import_trans_time, min_time=G_MAXINT64, max_time=0;
    GList* all_accounts = NULL;
    static const int secs_per_day = 86400;
    GSList* imported_trans;
    GList* potential_match;
    GHashTable* lists_per_accounts = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, (GDestroyNotify)g_slist_free);

    // Go through all imported transactions, gather the list of accounts, and min/max date range.
    for (imported_trans=gui->temp_trans_list; imported_trans!=NULL; imported_trans=imported_trans->next)
    {
        GNCImportTransInfo* transaction_info;
        transaction_info = imported_trans->data;
        import_trans_account = xaccSplitGetAccount (gnc_import_TransInfo_get_fsplit (transaction_info));
        all_accounts = g_list_prepend (all_accounts, import_trans_account);
        import_trans_time = xaccTransGetDate (gnc_import_TransInfo_get_trans (transaction_info));
        min_time = MIN (min_time, import_trans_time);
        max_time = MAX (max_time, import_trans_time);
    }

    // Make a query to find splits with the right accounts and dates.
    qof_query_set_book (query, gnc_get_current_book());
    xaccQueryAddAccountMatch (query, all_accounts,
                              QOF_GUID_MATCH_ANY, QOF_QUERY_AND);
    xaccQueryAddDateMatchTT (query,
                             TRUE, min_time - match_date_hardlimit * secs_per_day,
                             TRUE, max_time + match_date_hardlimit * secs_per_day,
                             QOF_QUERY_AND);
    query_return = qof_query_run (query);
    g_list_free (all_accounts);
    
    // Now put all potential matches into a hash table based on their account.
    for (potential_match=query_return; potential_match!=NULL; potential_match=potential_match->next)
    {
        Account* split_account;
        GSList* per_account_list;
        if (gnc_import_split_has_online_id (potential_match->data))
            continue;
        split_account = xaccSplitGetAccount (potential_match->data);
        // g_hash_table_steal_extended would do the two calls in one shot but is not available until 2.58
        per_account_list = g_hash_table_lookup (lists_per_accounts, import_trans_account);
        g_hash_table_steal (lists_per_accounts, import_trans_account);
        per_account_list = g_slist_prepend (per_account_list, potential_match->data);
        g_hash_table_insert (lists_per_accounts, import_trans_account, per_account_list);
    }
    return lists_per_accounts;
}

typedef struct _match_struct
{
    GNCImportTransInfo* transaction_info;
    gint display_threshold;
    double fuzzy_amount;
} match_struct;

static void
match_helper (Split* data, match_struct* s)
{
    split_find_match (s->transaction_info, data, s->display_threshold, s->fuzzy_amount);
}

/* This goes through the temporary list of imported transactions, runs a single query to gater potential matching
 * register transactions based on accounts and dates, then selects potential matching transactions for
 * each imported transaction. Doing a single query makes things a lot faster than doing one query per
 * imported transaction.
 */
void
gnc_gen_trans_list_create_matches (GNCImportMainMatcher *gui)
{
    GtkTreeModel* model = gtk_tree_view_get_model (gui->view);
    GtkListStore* store = GTK_LIST_STORE (model);
    GtkTreeIter iter;
    GNCImportMatchInfo *selected_match;
    gboolean match_selected_manually;
    GHashTable* lists_per_accounts = NULL;
    Query *query;
    gint match_date_hardlimit = gnc_import_Settings_get_match_date_hardlimit (gui->user_settings);
    gint display_threshold = gnc_import_Settings_get_display_threshold (gui->user_settings);
    double fuzzy_amount = gnc_import_Settings_get_fuzzy_amount (gui->user_settings);
    GSList* imported_trans;

    query = qof_query_create_for (GNC_ID_SPLIT);
    // Gather the list of splits that could match one of the imported transactions, this return a hash table.
    lists_per_accounts = create_list_of_potential_matches (gui, model, query, match_date_hardlimit);

    // For each imported transaction, grab the list of potential matches corresponding to that account,
    // and evaluate how good a match they are.
    for (imported_trans=gui->temp_trans_list; imported_trans!=NULL; imported_trans=imported_trans->next)
    {
        GNCImportTransInfo* transaction_info = imported_trans->data;
        Account *importaccount = xaccSplitGetAccount (gnc_import_TransInfo_get_fsplit (transaction_info));
        match_struct s = {transaction_info, display_threshold, fuzzy_amount};
        
        g_slist_foreach (g_hash_table_lookup (lists_per_accounts, importaccount), (GFunc) match_helper, &s);

        // Sort the matches, select the best match, and set the action.
        gnc_import_TransInfo_init_matches (transaction_info, gui->user_settings);

        selected_match = gnc_import_TransInfo_get_selected_match (transaction_info);
        match_selected_manually = gnc_import_TransInfo_get_match_selected_manually (transaction_info);

        if (selected_match)
            gnc_import_PendingMatches_add_match (gui->pending_matches,
                                                 selected_match,
                                                 match_selected_manually);
        
        gtk_tree_store_append (GTK_TREE_STORE (model), &iter, NULL);
        refresh_model_row (gui, model, &iter, transaction_info);
    }
    
    qof_query_destroy (query);
    g_hash_table_destroy (lists_per_accounts);
    return;
}

GtkWidget *gnc_gen_trans_list_widget (GNCImportMainMatcher *info)
{
    g_assert (info);
    return info->main_widget;
}

/** @} */
