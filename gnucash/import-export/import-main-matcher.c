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
#include "gnc-glib-utils.h"
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
    GtkTreeViewColumn       *account_column;
    GtkTreeViewColumn       *memo_column;
    GtkWidget               *show_account_column;
    GtkWidget               *show_matched_info;
    GtkWidget               *append_text; // Update+Clear: Append import Desc/Notes to matched Desc/Notes
    GtkWidget               *reconcile_after_close;
    gboolean add_toggled;     // flag to indicate that add has been toggled to stop selection
    gint id;
    GSList* temp_trans_list;  // Temporary list of imported transactions
    GHashTable* acct_id_hash; // Hash table, per account, of list of transaction IDs.
    GSList* edited_accounts;  // List of accounts currently edited.

    /* only when editing fields */
    gboolean edit_desc;
    gboolean edit_notes;
    gboolean edit_memo;

    GList *desc_list;
    GList *notes_list;
    GList *memo_list;

    GList *new_strings;
};

enum downloaded_cols
{
    DOWNLOADED_COL_DATE_TXT = 0,
    DOWNLOADED_COL_DATE_INT64, // used only for sorting
    DOWNLOADED_COL_ACCOUNT,
    DOWNLOADED_COL_AMOUNT,
    DOWNLOADED_COL_AMOUNT_DOUBLE, // used only for sorting
    DOWNLOADED_COL_DESCRIPTION,
    DOWNLOADED_COL_DESCRIPTION_ORIGINAL,
    DOWNLOADED_COL_DESCRIPTION_STYLE,
    DOWNLOADED_COL_MEMO,
    DOWNLOADED_COL_MEMO_ORIGINAL,
    DOWNLOADED_COL_MEMO_STYLE,
    DOWNLOADED_COL_NOTES_ORIGINAL,
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

static const gpointer one = GINT_TO_POINTER (1);

void on_matcher_ok_clicked (GtkButton *button, GNCImportMainMatcher *info);
void on_matcher_cancel_clicked (GtkButton *button, gpointer user_data);
gboolean on_matcher_delete_event (GtkWidget *widget, GdkEvent *event, gpointer data);
void on_matcher_help_clicked (GtkButton *button, gpointer user_data);
void on_matcher_help_close_clicked (GtkButton *button, gpointer user_data);

static void gnc_gen_trans_list_create_matches (GNCImportMainMatcher *gui);

/* Local prototypes */
static void gnc_gen_trans_assign_transfer_account (GtkTreeView *treeview,
                                                   gboolean *first,
                                                   gboolean is_selection,
                                                   GtkTreePath *path,
                                                   Account **new_acc,
                                                   GNCImportMainMatcher *info);
static void gnc_gen_trans_assign_transfer_account_to_selection_cb (GtkMenuItem *menuitem,
                                                                   GNCImportMainMatcher *info);
static void gnc_gen_trans_view_popup_menu (GtkTreeView *treeview,
                                           GdkEvent *event,
                                           GNCImportMainMatcher *info);
static gboolean gnc_gen_trans_onButtonPressed_cb (GtkTreeView *treeview,
                                                  GdkEvent *event,
                                                  GNCImportMainMatcher *info);
static gboolean gnc_gen_trans_onPopupMenu_cb (GtkTreeView *treeview,
                                              GNCImportMainMatcher *info);
static void refresh_model_row (GNCImportMainMatcher *gui,
                               GtkTreeModel *model,
                               GtkTreeIter *iter,
                               GNCImportTransInfo *info);
static gboolean query_tooltip_tree_view_cb (GtkWidget *widget, gint x, gint y,
                                            gboolean keyboard_tip,
                                            GtkTooltip *tooltip,
                                            gpointer user_data);
/* end local prototypes */

static void
update_all_balances (GNCImportMainMatcher *info)
{
    for (GSList* iter = info->edited_accounts; iter; iter=iter->next)
    {
        gnc_account_set_defer_bal_computation (iter->data,FALSE);
        xaccAccountRecomputeBalance (iter->data);
    }
    g_slist_free (info->edited_accounts);
    info->edited_accounts = NULL;
}

static void
defer_bal_computation (GNCImportMainMatcher *info, Account* acc)
{
    if (!gnc_account_get_defer_bal_computation (acc))
    {
        gnc_account_set_defer_bal_computation (acc, TRUE);
        info->edited_accounts = g_slist_prepend (info->edited_accounts, acc);
    }
}

typedef struct
{
    const char *str;
    char *collated;
    char *normalized_folded;
} StringInfo;

static inline StringInfo*
make_stringinfo (const char *str)
{
    StringInfo *elt = g_new (StringInfo, 1);
    char *normalized = g_utf8_normalize (str, -1, G_NORMALIZE_ALL);
    elt->str = str;
    elt->collated = g_utf8_collate_key (str, -1);
    elt->normalized_folded = normalized ? g_utf8_casefold (normalized, -1) : NULL;
    g_free (normalized);
    return elt;
}

static int stringinfo_eq (StringInfo *info, const gchar *str)
{
    return g_strcmp0 (info->str, str);
}

static gint stringinfo_cmp (StringInfo *la, StringInfo *lb)
{
    if (la)
        return lb ? g_strcmp0 (la->collated, lb->collated) : 1;
    else
        return lb ? -1 : 0;
}

static void stringinfo_free (StringInfo *a)
{
    g_free (a->collated);
    g_free (a->normalized_folded);
    g_free (a);
}

void
gnc_gen_trans_list_delete (GNCImportMainMatcher *info)
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
        gtk_widget_destroy (GTK_WIDGET(info->main_widget));
    }
    else
        gnc_import_Settings_delete (info->user_settings);

    g_slist_free_full (info->temp_trans_list, (GDestroyNotify) gnc_import_TransInfo_delete);
    info->temp_trans_list = NULL;

    // We've deferred balance computations on many accounts. Let's do it now that we're done.
    update_all_balances (info);

    gnc_import_PendingMatches_delete (info->pending_matches);
    g_hash_table_destroy (info->acct_id_hash);

    g_list_free_full (info->desc_list, (GDestroyNotify)stringinfo_free);
    g_list_free_full (info->notes_list, (GDestroyNotify)stringinfo_free);
    g_list_free_full (info->memo_list, (GDestroyNotify)stringinfo_free);
    g_list_free_full (info->new_strings, (GDestroyNotify)g_free);

    g_free (info);
}

gboolean
gnc_gen_trans_list_empty (GNCImportMainMatcher *info)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;
    g_assert (info);
    model = gtk_tree_view_get_model (info->view);
    // Check that both the tree model and the temporary list are empty.
    return !gtk_tree_model_get_iter_first (model, &iter) && !info->temp_trans_list;
}

static void
gnc_gen_trans_list_show_accounts_column (GNCImportMainMatcher *info)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;
    gboolean multiple_accounts = FALSE;
    gboolean valid;

    g_assert (info);

    model = gtk_tree_view_get_model (info->view);

    if (gtk_tree_model_iter_n_children (model, NULL) > 1)
    {
        /* Get first row in list store */
        valid = gtk_tree_model_get_iter_first (model, &iter);
        if (valid)
        {
            gchar *account_name;
            gtk_tree_model_get (model, &iter, DOWNLOADED_COL_ACCOUNT, &account_name, -1);

            valid = gtk_tree_model_iter_next (model, &iter);

            while (valid)
            {
                gchar *test_account_name;

                gtk_tree_model_get (model, &iter, DOWNLOADED_COL_ACCOUNT, &test_account_name, -1);
                if (g_strcmp0 (account_name, test_account_name) != 0)
                {
                     multiple_accounts = TRUE;
                     g_free (test_account_name);
                     break;
                }
                valid = gtk_tree_model_iter_next (model, &iter);
                g_free (test_account_name);
            }
            g_free (account_name);
        }
        // now toggle the column
        if (multiple_accounts)
        {
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->show_account_column), TRUE);
            gtk_tree_view_expand_all (info->view);
        }
        else
        {
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->show_account_column), FALSE);
            gtk_tree_view_collapse_all (info->view);
        }
    }
}

// This returns the transaction ID of the first match candidate in match_list
static const GncGUID*
get_top_trans_match_id (GList* match_list)
{
    Transaction* trans = NULL;
    GNCImportMatchInfo* match_info;
    if (!match_list || !match_list->data) return NULL;
    match_info = match_list->data;
    trans = match_info->trans;
    return xaccTransGetGUID (trans);
}

// This returns the transaction score of the first match candidate in match_list
static gint
get_top_trans_match_score (GList* match_list)
{
    GNCImportMatchInfo* match_info;
    if (!match_list || !match_list->data) return 0;
    match_info = match_list->data;
    return match_info->probability;
}

static GList*
get_trans_match_list (GtkTreeModel* model, GtkTreeIter* iter)
{
    GNCImportTransInfo* transaction_info;
    gtk_tree_model_get (model, iter,
                        DOWNLOADED_COL_DATA, &transaction_info,
                        -1);
    return gnc_import_TransInfo_get_match_list (transaction_info);
}

static GNCImportTransInfo*
get_trans_info (GtkTreeModel* model, GtkTreeIter* iter)
{
    GNCImportTransInfo* transaction_info;
    gtk_tree_model_get (model, iter,
                        DOWNLOADED_COL_DATA, &transaction_info,
                        -1);
    return transaction_info;
}
/* This function finds the top matching register transaction for the imported transaction pointed to by iter
 * It then goes through the list of all other imported transactions and creates a list of the ones that
 * have the same register transaction as their top match (i.e., are in conflict). It finds the best of them
 * (match-score-wise) and returns the rest as a list. The imported transactions in that list will get their
 * top match modified. */
static GList*
get_conflict_list (GtkTreeModel* model, GtkTreeIter import_iter, GncGUID* id, gint best_match)
{
    GtkTreeIter iter = import_iter;
    GNCImportTransInfo* best_import = get_trans_info (model, &import_iter);
    GList* conflicts = g_list_prepend (NULL, best_import);

    while (gtk_tree_model_iter_next (model, &iter))
    {
        gint match_score = 0;
        GNCImportTransInfo* trans_info;
        GncGUID id2;
        // Get the ID of the top matching trans for this imported trans.
        GList* register_iter = get_trans_match_list (model, &iter);
        if (!register_iter || !register_iter->data)
            continue;

        id2 = *get_top_trans_match_id (register_iter);
        if (!guid_equal (id, &id2))
            continue;

        // Conflict. Get the match score, add this transaction to our list.
        match_score = get_top_trans_match_score (register_iter);
        trans_info = get_trans_info (model, &iter);
        conflicts = g_list_prepend (conflicts, trans_info);

        if (match_score > best_match)
        {
            // Keep track of the imported transaction with the best score.
            best_match = match_score;
            best_import = trans_info;
        }
    }

    // Remove the best match from the list of conflicts, as it will keep its match
    conflicts = g_list_remove (conflicts, best_import);
    return conflicts;
}

static void
remove_top_matches (GNCImportMainMatcher* gui, GtkTreeModel* model, GList* conflicts)
{
    GList* iter = conflicts;
    for (; iter && iter->data; iter=iter->next)
    {
        GNCImportTransInfo* trans_info = iter->data;
        GList* match_trans = gnc_import_TransInfo_get_match_list (trans_info);
        match_trans = g_list_remove (match_trans, match_trans->data);
        gnc_import_TransInfo_set_match_list (trans_info, match_trans);
    }

    g_list_free (conflicts);
}

static void
resolve_conflicts (GNCImportMainMatcher *info)
{
    GtkTreeModel* model = gtk_tree_view_get_model (info->view);
    GtkTreeIter import_iter, best_import;
    gint best_match = 0;

    /* A greedy conflict resolution. Find all imported trans that vie for the same
     * register trans. Assign the reg trans to the imported trans with the best match.
     * Loop over the imported transactions */
    gboolean valid = gtk_tree_model_get_iter_first (model, &import_iter);
    while (valid)
    {
        GList* conflicts = NULL;
        GncGUID id;
        GList* match_list = get_trans_match_list (model, &import_iter);
        if (!match_list || !match_list->data)
        {
            valid = gtk_tree_model_iter_next (model, &import_iter);
            continue;
        }

        // The ID of the best current match for this imported trans
        id = *get_top_trans_match_id (match_list);
        best_match = get_top_trans_match_score (match_list);
        best_import = import_iter;
        /* Get a list of all imported transactions that have a conflict with this one.
         * The returned list excludes the best transaction. */
        conflicts = get_conflict_list (model, import_iter, &id, best_match);

        if (conflicts)
        {
            remove_top_matches (info, model, conflicts);
            /* Go back to the beginning here, because a nth choice
             * could now conflict with a previously assigned first choice. */
            valid = gtk_tree_model_get_iter_first (model, &import_iter);
        }
        else
            valid = gtk_tree_model_iter_next (model, &import_iter);
        /* NOTE: The loop is guaranteed to terminate because whenever we go back to the top
         * we remove at least 1 match, and there's a finite number of them. */
    }

    // Refresh all
    valid = gtk_tree_model_get_iter_first (model, &import_iter);
    while (valid)
    {
        refresh_model_row (info, model, &import_iter, get_trans_info (model, &import_iter));
        valid = gtk_tree_model_iter_next (model, &import_iter);
    }
}

static void add_stringinfo_list (gpointer key, gpointer value, GList **list)
{
    StringInfo *elt = make_stringinfo (key);
    *list = g_list_prepend (*list, elt);
}

static void
load_stringinfo_lists (GNCImportMainMatcher *info)
{
    GtkTreeModel *model = gtk_tree_view_get_model (info->view);
    GtkTreeIter import_iter;
    GList *accounts_list = NULL;
    gboolean valid = gtk_tree_model_get_iter_first (model, &import_iter);
    GHashTable *desc_hash = g_hash_table_new (g_str_hash, g_str_equal);
    GHashTable *notes_hash = g_hash_table_new (g_str_hash, g_str_equal);
    GHashTable *memo_hash = g_hash_table_new (g_str_hash, g_str_equal);
    while (valid)
    {
        GNCImportTransInfo *trans_info = get_trans_info (model, &import_iter);
        Split *s = gnc_import_TransInfo_get_fsplit (trans_info);
        Account *acc = xaccSplitGetAccount (s);
        if (!g_list_find (accounts_list, acc))
            accounts_list = g_list_prepend (accounts_list, acc);
        valid = gtk_tree_model_iter_next (model, &import_iter);
    }
    for (GList *m = accounts_list; m; m = m->next)
    {
        for (GList *n = xaccAccountGetSplitList (m->data); n; n = n->next)
        {
            const Split *s = n->data;
            const Transaction *t = xaccSplitGetParent (s);
            const gchar *key;

            key = xaccTransGetDescription (t);
            if (key && *key)
                g_hash_table_insert (desc_hash, (gpointer)key, one);

            key = xaccTransGetNotes (t);
            if (key && *key)
                g_hash_table_insert (notes_hash, (gpointer)key, one);

            key = xaccSplitGetMemo (s);
            if (key && *key)
                g_hash_table_insert (memo_hash, (gpointer)key, one);
        }
    }

    g_hash_table_foreach (desc_hash, (GHFunc)add_stringinfo_list, &info->desc_list);
    g_hash_table_foreach (notes_hash, (GHFunc)add_stringinfo_list, &info->notes_list);
    g_hash_table_foreach (memo_hash, (GHFunc)add_stringinfo_list, &info->memo_list);

    g_hash_table_destroy (desc_hash);
    g_hash_table_destroy (notes_hash);
    g_hash_table_destroy (memo_hash);

    info->desc_list = g_list_sort (info->desc_list, (GCompareFunc) stringinfo_cmp);
    info->notes_list = g_list_sort (info->notes_list, (GCompareFunc) stringinfo_cmp);
    info->memo_list = g_list_sort (info->memo_list, (GCompareFunc) stringinfo_cmp);

    g_list_free (accounts_list);
}

void
gnc_gen_trans_list_show_all (GNCImportMainMatcher *info)
{
    GNCImportTransInfo* trans_info;
    Account* account;
    Split* first_split;
    GSList* temp_trans_list;

    g_assert (info);

    // Set initial state of Append checkbox to same as last import for this account.
    // Get the import account from the first split in first transaction.
    temp_trans_list = info->temp_trans_list;
    if (!temp_trans_list)
    {
        gnc_info_dialog (GTK_WINDOW (info->main_widget), _("No new transactions were found in this import."));
        return;
    }
    trans_info = temp_trans_list->data;
    first_split = gnc_import_TransInfo_get_fsplit (trans_info);
    account = xaccSplitGetAccount(first_split);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (info->append_text),
                                 xaccAccountGetAppendText(account));

    gnc_gen_trans_list_create_matches (info);
    load_stringinfo_lists (info);
    resolve_conflicts (info);
    gtk_widget_show_all (GTK_WIDGET(info->main_widget));
    gnc_gen_trans_list_show_accounts_column (info);
}

void
on_matcher_ok_clicked (GtkButton *button, GNCImportMainMatcher *info)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNCImportTransInfo *trans_info;
    gboolean append_text = gtk_toggle_button_get_active ((GtkToggleButton*) info->append_text);
    gboolean first_tran = TRUE;
    gpointer user_data = info->user_data;

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
    gnc_suspend_gui_refresh ();
    do
    {
        gtk_tree_model_get (model, &iter,
                            DOWNLOADED_COL_DATA, &trans_info,
                            -1);

        // Allow the backend to know if the Append checkbox is ticked or unticked
        // by propagating info->append_text to every trans_info->append_text
        gnc_import_TransInfo_set_append_text( trans_info, append_text);

        // When processing the first transaction,
        // save the state of the Append checkbox to an account kvp so the same state can be
        //  defaulted next time this account is imported.
        // Get the import account from the first split.
        if (first_tran)
        {
            Split* first_split = gnc_import_TransInfo_get_fsplit (trans_info);
            xaccAccountSetAppendText (xaccSplitGetAccount(first_split), append_text);
            first_tran = FALSE;
        }
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
    gnc_resume_gui_refresh ();

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

    builder = gtk_builder_new ();
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
    gchar *acct_str = gnc_get_account_name_for_register (*new_acc);

    ENTER("");
    DEBUG("first = %s", *first ? "true" : "false");
    DEBUG("is_selection = %s", is_selection ? "true" : "false");
    DEBUG("path  = %s", path_str);
    g_free (path_str);
    DEBUG("account passed in = %s", acct_str);
    g_free (acct_str);

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
                    gchar *acc_full_name = gnc_account_get_full_name (*new_acc);
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
                    acc_full_name = gnc_account_get_full_name (*new_acc);
                    DEBUG("account selected = %s", acc_full_name);
                    g_free (acc_full_name);
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
gnc_gen_trans_assign_transfer_account_to_selection_cb (GtkMenuItem *menuitem,
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
            gchar *fullname;
            DEBUG("passing first = %s", first ? "true" : "false");
            DEBUG("passing is_selection = %s", is_selection ? "true" : "false");
            DEBUG("passing path = %s", path_str);
            g_free (path_str);
            refs = g_list_prepend (refs, ref);
            fullname = gnc_account_get_full_name (assigned_account);
            DEBUG("passing account value = %s", fullname);
            g_free (fullname);
            gnc_gen_trans_assign_transfer_account (treeview,
                                                   &first, is_selection, l->data,
                                                   &assigned_account, info);
            fullname = gnc_account_get_full_name (assigned_account);
            DEBUG("returned value of account = %s", fullname);
            DEBUG("returned value of first = %s", first ? "true" : "false");
            g_free (fullname);
            if (assigned_account == NULL)
                break;

        }
    }
    g_list_free_full (selected_rows, (GDestroyNotify)gtk_tree_path_free);

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

typedef struct
{
    Split *split;
    Transaction *trans;
    GtkTreeIter iter;
    char *orig_desc, *orig_notes, *orig_memo;
} RowInfo;

static void rowinfo_free (RowInfo* info)
{
    g_free (info->orig_desc);
    g_free (info->orig_notes);
    g_free (info->orig_memo);
    g_free (info);
}

static RowInfo * row_get_info (gpointer row, GNCImportMainMatcher *info)
{
    GNCImportTransInfo *trans_info;
    GtkTreeModel *model = gtk_tree_view_get_model (info->view);
    RowInfo *retval = g_new (RowInfo, 1);
    gtk_tree_model_get_iter (model, &retval->iter, row);
    gtk_tree_model_get (model, &retval->iter,
                        DOWNLOADED_COL_DATA, &trans_info,
                        DOWNLOADED_COL_DESCRIPTION_ORIGINAL, &retval->orig_desc,
                        DOWNLOADED_COL_NOTES_ORIGINAL, &retval->orig_notes,
                        DOWNLOADED_COL_MEMO_ORIGINAL, &retval->orig_memo,
                        -1);
    retval->trans = gnc_import_TransInfo_get_trans (trans_info);
    retval->split = gnc_import_TransInfo_get_fsplit (trans_info);
    return retval;
}

enum
{
    COMPLETION_LIST_ORIGINAL,
    COMPLETION_LIST_NORMALIZED_FOLDED,
    NUM_COMPLETION_COLS
};

static void addto_list_store (StringInfo *info, GtkListStore *list)
{
    GtkTreeIter iter;
    gtk_list_store_append (list, &iter);
    gtk_list_store_set (list, &iter,
                        COMPLETION_LIST_ORIGINAL, info->str,
                        COMPLETION_LIST_NORMALIZED_FOLDED, info->normalized_folded,
                        -1);
}

static gboolean
match_func (GtkEntryCompletion *completion, const char *entry_str,
            GtkTreeIter *iter, gpointer user_data)
{
    GtkTreeModel *model = user_data;
    gchar *existing_str = NULL;
    gboolean ret = FALSE;
    gtk_tree_model_get (model, iter,
                        COMPLETION_LIST_NORMALIZED_FOLDED, &existing_str,
                        -1);
    if (existing_str && *existing_str && strstr (existing_str, entry_str))
        ret = TRUE;
    g_free (existing_str);
    return ret;
}

static void
setup_entry (GtkWidget *entry, gboolean sensitive, GList *existing_list,
             const char *initial)
{
    GtkEntryCompletion* completion;
    GtkListStore *list;

    gtk_entry_set_text (GTK_ENTRY (entry), sensitive ? initial : _("Disabled"));
    gtk_widget_set_sensitive (entry, sensitive);
    if (!sensitive)
        return;

    list = gtk_list_store_new (NUM_COMPLETION_COLS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
    if (initial && *initial && !g_list_find_custom (existing_list, initial,
                                                    (GCompareFunc)stringinfo_eq))
    {
        StringInfo *info = make_stringinfo (initial);
        addto_list_store (info, list);
        stringinfo_free (info);
    }
    g_list_foreach (existing_list, (GFunc)addto_list_store, list);
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (list),
                                          COMPLETION_LIST_ORIGINAL,
                                          GTK_SORT_ASCENDING);

    completion = gtk_entry_completion_new ();
    gtk_entry_completion_set_model (completion, GTK_TREE_MODEL(list));
    gtk_entry_completion_set_text_column (completion, COMPLETION_LIST_ORIGINAL);
    gtk_entry_completion_set_match_func (completion,
                                         (GtkEntryCompletionMatchFunc)match_func,
                                         GTK_TREE_MODEL(list), NULL);
    gtk_entry_set_completion (GTK_ENTRY (entry), completion);
}

static gboolean
input_new_fields (GNCImportMainMatcher *info, RowInfo *rowinfo,
                  char **new_desc, char **new_notes, char **new_memo)
{
    GtkWidget  *desc_entry, *notes_entry, *memo_entry, *label;
    GtkWidget  *dialog;
    GtkBuilder *builder;
    gboolean    retval = FALSE;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_edit_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_edit_dialog"));

    desc_entry = GTK_WIDGET(gtk_builder_get_object (builder, "desc_entry"));
    memo_entry = GTK_WIDGET(gtk_builder_get_object (builder, "memo_entry"));
    notes_entry = GTK_WIDGET(gtk_builder_get_object (builder, "notes_entry"));

    setup_entry (desc_entry, info->edit_desc, info->desc_list, xaccTransGetDescription (rowinfo->trans));
    setup_entry (notes_entry, info->edit_notes, info->notes_list, xaccTransGetNotes (rowinfo->trans));
    setup_entry (memo_entry, info->edit_memo, info->memo_list, xaccSplitGetMemo (rowinfo->split));

    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (info->main_widget));

    // run the dialog
    gtk_widget_show_all (dialog);

    switch (gtk_dialog_run (GTK_DIALOG(dialog)))
    {
    case GTK_RESPONSE_OK:
        *new_desc = g_strdup (gtk_entry_get_text (GTK_ENTRY (desc_entry)));
        *new_notes = g_strdup (gtk_entry_get_text (GTK_ENTRY (notes_entry)));
        *new_memo = g_strdup (gtk_entry_get_text (GTK_ENTRY (memo_entry)));
        retval = TRUE;
        break;
    default:
        break;
    }

    gtk_widget_destroy (dialog);
    g_object_unref (G_OBJECT(builder));
    return retval;
}

static void
gnc_gen_trans_edit_fields (GtkMenuItem *menuitem, GNCImportMainMatcher *info)
{
    GtkTreeView *treeview;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GList *selected_rows, *row_info_list;
    GtkTreeStore* store;
    char *new_desc = NULL, *new_notes = NULL, *new_memo = NULL;

    g_return_if_fail (info != NULL);
    ENTER("assign_transfer_account_to_selection_cb");

    treeview = GTK_TREE_VIEW(info->view);
    model = gtk_tree_view_get_model (treeview);
    store  = GTK_TREE_STORE (model);
    selection = gtk_tree_view_get_selection (treeview);
    selected_rows = gtk_tree_selection_get_selected_rows (selection, &model);

    if (!selected_rows)
    {
        LEAVE ("No selected rows");
        return;
    }

    row_info_list = gnc_g_list_map (selected_rows, (GncGMapFunc) row_get_info, info);

    if (input_new_fields (info, row_info_list->data,
                          &new_desc, &new_notes, &new_memo))
    {
        for (GList *n = row_info_list; n; n = g_list_next (n))
        {
            RowInfo *row = n->data;
            if (info->edit_desc)
            {
                gint style = g_strcmp0 (new_desc, row->orig_desc) ?
                    PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL;
                gtk_tree_store_set (store, &row->iter,
                                    DOWNLOADED_COL_DESCRIPTION, new_desc,
                                    DOWNLOADED_COL_DESCRIPTION_STYLE, style,
                                    -1);
                xaccTransSetDescription (row->trans, new_desc);
                if (*new_desc && !g_list_find_custom (info->desc_list, new_desc,
                                                      (GCompareFunc)stringinfo_eq))
                {
                    char *new_string = g_strdup (new_desc);
                    info->new_strings = g_list_prepend (info->new_strings, new_string);
                    info->desc_list = g_list_insert_sorted
                        (info->desc_list, make_stringinfo (new_string),
                         (GCompareFunc) stringinfo_cmp);
                }
            }

            if (info->edit_notes)
            {
                xaccTransSetNotes (row->trans, new_notes);
                if (*new_notes && !g_list_find_custom (info->notes_list, new_notes,
                                                      (GCompareFunc)stringinfo_eq))
                {
                    char *new_string = g_strdup (new_notes);
                    info->new_strings = g_list_prepend (info->new_strings, new_string);
                    info->notes_list = g_list_insert_sorted
                        (info->notes_list, make_stringinfo (new_string),
                         (GCompareFunc) stringinfo_cmp);
                }
            }

            if (info->edit_memo)
            {
                gint style = g_strcmp0 (new_memo, row->orig_memo) ?
                    PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL;
                gtk_tree_store_set (store, &row->iter,
                                    DOWNLOADED_COL_MEMO, new_memo,
                                    DOWNLOADED_COL_MEMO_STYLE, style,
                                    -1);
                xaccSplitSetMemo (row->split, new_memo);
                if (*new_memo && !g_list_find_custom (info->memo_list, new_memo,
                                                      (GCompareFunc)stringinfo_eq))
                {
                    char *new_string = g_strdup (new_memo);
                    info->new_strings = g_list_prepend (info->new_strings, new_string);
                    info->memo_list = g_list_insert_sorted
                        (info->memo_list, make_stringinfo (new_string),
                         (GCompareFunc) stringinfo_cmp);
                }
            }
        }
        g_free (new_desc);
        g_free (new_memo);
        g_free (new_notes);
    }
    g_list_free_full (row_info_list, (GDestroyNotify)rowinfo_free);
    g_list_free_full (selected_rows, (GDestroyNotify)gtk_tree_path_free);
    LEAVE("");
}

static void
gnc_gen_trans_reset_edits_cb (GtkMenuItem *menuitem, GNCImportMainMatcher *info)
{
    GtkTreeView *treeview;
    GtkTreeModel *model;
    GtkTreeStore *store;
    GtkTreeSelection *selection;
    GList *selected_rows, *row_info_list;

    g_return_if_fail (info != NULL);
    ENTER("gnc_gen_trans_reset_edits_cb");

    treeview = GTK_TREE_VIEW(info->view);
    model = gtk_tree_view_get_model (treeview);
    store  = GTK_TREE_STORE (model);
    selection = gtk_tree_view_get_selection (treeview);
    selected_rows = gtk_tree_selection_get_selected_rows (selection, &model);

    if (!selected_rows)
    {
        LEAVE ("No selected rows");
        return;
    }

    for (GList *n = selected_rows; n; n = g_list_next (n))
    {
        RowInfo *rowinfo = row_get_info (n->data, info);
        xaccTransSetDescription (rowinfo->trans, rowinfo->orig_desc);
        xaccTransSetNotes (rowinfo->trans, rowinfo->orig_notes);
        xaccSplitSetMemo (rowinfo->split, rowinfo->orig_memo);
        gtk_tree_store_set (store, &rowinfo->iter,
                            DOWNLOADED_COL_DESCRIPTION, rowinfo->orig_desc,
                            DOWNLOADED_COL_MEMO, rowinfo->orig_memo,
                            DOWNLOADED_COL_DESCRIPTION_STYLE, PANGO_STYLE_NORMAL,
                            DOWNLOADED_COL_MEMO_STYLE, PANGO_STYLE_NORMAL,
                            -1);
        rowinfo_free (rowinfo);
    };
    g_list_free_full (selected_rows, (GDestroyNotify)gtk_tree_path_free);
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
    gchar *namestr;

    ENTER("");
    assigned_account = NULL;
    first = TRUE;
    is_selection = FALSE;
    gnc_gen_trans_assign_transfer_account (treeview,
                                           &first, is_selection, path,
                                           &assigned_account, info);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (treeview), path);

    namestr = gnc_account_get_full_name (assigned_account);
    DEBUG("account returned = %s", namestr);
    g_free (namestr);
    LEAVE("");
}

static GNCImportAction
get_action_for_path (GtkTreePath* path, GtkTreeModel *model)
{
    GNCImportTransInfo *trans_info;
    GtkTreeIter iter;
    gtk_tree_model_get_iter (model, &iter, path);
    gtk_tree_model_get (model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);
    if (!trans_info)
          // selected row is a potential match  (depth 2)
          // instead of an imported transaction (depth 1)
          return GNCImport_INVALID_ACTION;
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
        for (GList *n = list; n; n = n->next)
        {
            if (get_action_for_path (n->data, model) != GNCImport_ADD)
                gtk_tree_selection_unselect_path (selection, n->data);
        }
        g_list_free_full (list, (GDestroyNotify)gtk_tree_path_free);
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
    GtkTreeModel *model;
    GtkTreeSelection *selection;
    GList *selected_rows;
    const char *desc = NULL, *memo = NULL, *notes = NULL;
    gboolean has_edits = FALSE;

    ENTER ("");
    menu = gtk_menu_new();
    menuitem = gtk_menu_item_new_with_mnemonic (
                   /* Translators: Menu entry, no full stop */
                   _("_Assign a transfer account to the selection"));
    g_signal_connect (menuitem, "activate",
                      G_CALLBACK(
                      gnc_gen_trans_assign_transfer_account_to_selection_cb),
                      info);
    DEBUG("Callback to assign destination account to selection connected");
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), menuitem);

    model = gtk_tree_view_get_model (treeview);
    selection = gtk_tree_view_get_selection (treeview);
    selected_rows = gtk_tree_selection_get_selected_rows (selection, &model);

    /* initialise */
    info->edit_desc = TRUE;
    info->edit_notes = TRUE;
    info->edit_memo = TRUE;

    for (GList *n = selected_rows;
         (!has_edits || info->edit_desc || info->edit_notes || info->edit_memo) && n;
         n = g_list_next(n))
    {
        RowInfo *rowinfo = row_get_info (n->data, info);

        if (!has_edits &&
            (g_strcmp0 (xaccSplitGetMemo (rowinfo->split), rowinfo->orig_memo) ||
             g_strcmp0 (xaccTransGetNotes (rowinfo->trans), rowinfo->orig_notes) ||
             g_strcmp0 (xaccTransGetDescription (rowinfo->trans), rowinfo->orig_desc)))
            has_edits = TRUE;

        if (!n->prev)       /* only the first row */
        {
            desc = xaccTransGetDescription (rowinfo->trans);
            notes = xaccTransGetNotes (rowinfo->trans);
            memo = xaccSplitGetMemo (rowinfo->split);
            rowinfo_free (rowinfo);
            continue;
        }
        if (info->edit_desc && g_strcmp0 (desc, xaccTransGetDescription (rowinfo->trans)))
            info->edit_desc = FALSE;
        if (info->edit_notes && g_strcmp0 (notes, xaccTransGetNotes (rowinfo->trans)))
            info->edit_notes = FALSE;
        if (info->edit_memo && g_strcmp0 (memo, xaccSplitGetMemo (rowinfo->split)))
            info->edit_memo = FALSE;
        rowinfo_free (rowinfo);
    }

    /* Translators: Menu entry, no full stop */
    menuitem = gtk_menu_item_new_with_mnemonic (_("_Edit description, notes, or memo"));
    gtk_widget_set_sensitive (menuitem,
                              info->edit_desc || info->edit_notes || info->edit_memo);
    g_signal_connect (menuitem, "activate",
                      G_CALLBACK (gnc_gen_trans_edit_fields),
                      info);
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), menuitem);

    /* Translators: Menu entry, no full stop */
    menuitem = gtk_menu_item_new_with_mnemonic (_("_Reset all edits"));
    gtk_widget_set_sensitive (menuitem, has_edits);
    g_signal_connect (menuitem, "activate",
                      G_CALLBACK (gnc_gen_trans_reset_edits_cb),
                      info);
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), menuitem);

    gtk_menu_attach_to_widget (GTK_MENU (menu), GTK_WIDGET (treeview), NULL);

    gtk_widget_show_all (menu);
    /* Note: event can be NULL here when called from view_onPopupMenu; */
    gtk_menu_popup_at_pointer (GTK_MENU(menu), (GdkEvent*)event);

    g_list_free_full (selected_rows, (GDestroyNotify)gtk_tree_path_free);
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
            if (count > 0)
            {
                GList* selected;
                GtkTreeModel *model;
                selected = gtk_tree_selection_get_selected_rows (selection, &model);
                if (get_action_for_path (selected->data, model) == GNCImport_ADD)
                    gnc_gen_trans_view_popup_menu (treeview, event, info);
                g_list_free_full (selected, (GDestroyNotify)gtk_tree_path_free);
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
add_text_column (GtkTreeView *view, const gchar *title, int col_num, gboolean ellipsize)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (title,
                                                       renderer,
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

    if (col_num == DOWNLOADED_COL_DESCRIPTION)
        gtk_tree_view_column_add_attribute (column, renderer, "style", DOWNLOADED_COL_DESCRIPTION_STYLE);

    if (col_num == DOWNLOADED_COL_MEMO)
        gtk_tree_view_column_add_attribute (column, renderer, "style", DOWNLOADED_COL_MEMO_STYLE);

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

    renderer = gtk_cell_renderer_toggle_new ();
    column = gtk_tree_view_column_new_with_attributes (title, renderer,
                                                       "active", col_num,
                                                       "cell-background", DOWNLOADED_COL_COLOR,
                                                       "activatable", DOWNLOADED_COL_ENABLE,
                                                       "visible", DOWNLOADED_COL_ENABLE,
                                                       NULL);
    gtk_tree_view_column_set_sort_column_id (column, col_num);
    g_object_set (G_OBJECT(column), "reorderable", TRUE, NULL);
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
                                G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT, //description stuff
                                G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT, //memo stuff
                                G_TYPE_STRING, G_TYPE_BOOLEAN,
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
    info->memo_column = add_text_column (view, _("Memo"), DOWNLOADED_COL_MEMO, TRUE);
    add_toggle_column (view, C_("Column header for 'Adding transaction'", "A"),
                       DOWNLOADED_COL_ACTION_ADD,
                       G_CALLBACK(gnc_gen_trans_add_toggled_cb), info);
    column = add_toggle_column (view, C_("Column header for 'Updating plus Clearing transaction'", "U+C"),
                                DOWNLOADED_COL_ACTION_UPDATE,
                                G_CALLBACK(gnc_gen_trans_update_toggled_cb), info);
    gtk_tree_view_column_set_visible (column, show_update);
    add_toggle_column (view, C_("Column header for 'Clearing transaction'", "C"),
                       DOWNLOADED_COL_ACTION_CLEAR,
                       G_CALLBACK(gnc_gen_trans_clear_toggled_cb), info);

    /* The last column has multiple renderers */
    renderer = gtk_cell_renderer_pixbuf_new ();
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

    g_object_set (info->view, "has-tooltip", TRUE, NULL);

    g_signal_connect (G_OBJECT(info->view), "query-tooltip",
                      G_CALLBACK(query_tooltip_tree_view_cb), info);
    g_signal_connect (info->view, "row-activated",
                      G_CALLBACK(gnc_gen_trans_row_activated_cb), info);
    g_signal_connect (selection, "changed",
                      G_CALLBACK(gnc_gen_trans_row_changed_cb), info);
    g_signal_connect (view, "button-press-event",
                      G_CALLBACK(gnc_gen_trans_onButtonPressed_cb), info);
    g_signal_connect (view, "popup-menu",
                      G_CALLBACK(gnc_gen_trans_onPopupMenu_cb), info);

    info->acct_id_hash = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL,
                                                (GDestroyNotify)g_hash_table_destroy);
}

static void
show_account_column_toggled_cb (GtkToggleButton *togglebutton,
                                GNCImportMainMatcher *info)
{
    gtk_tree_view_column_set_visible (info->account_column,
        gtk_toggle_button_get_active (togglebutton));
}

static void
show_memo_column_toggled_cb (GtkToggleButton *togglebutton,
                             GNCImportMainMatcher *info)
{
    gtk_tree_view_column_set_visible (info->memo_column,
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

static void
gnc_gen_trans_common_setup (GNCImportMainMatcher *info,
                            GtkWidget *parent,
                            GtkBuilder *builder,
                            const gchar* heading,
                            gboolean all_from_same_account,
                            gint match_date_hardlimit)
{
    GtkStyleContext *stylectxt;
    GdkRGBA color;
    GtkWidget *heading_label, *button;
    gboolean show_update;

    info->pending_matches = gnc_import_PendingMatches_new ();

    /* Initialize user Settings. */
    info->user_settings = gnc_import_Settings_new ();
    gnc_import_Settings_set_match_date_hardlimit (info->user_settings, match_date_hardlimit);

    stylectxt = gtk_widget_get_style_context (GTK_WIDGET(parent));
    gtk_style_context_get_color (stylectxt, GTK_STATE_FLAG_NORMAL, &color);
    info->dark_theme = gnc_is_dark_theme (&color);

    /* Get the view */
    info->view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "downloaded_view"));
    g_assert (info->view != NULL);

    info->show_account_column = GTK_WIDGET(gtk_builder_get_object (builder, "show_source_account_button"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->show_account_column), all_from_same_account);
    g_signal_connect (G_OBJECT(info->show_account_column), "toggled",
                      G_CALLBACK(show_account_column_toggled_cb), info);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "show_memo_column_button"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), TRUE);
    g_signal_connect (G_OBJECT(button), "toggled",
                      G_CALLBACK(show_memo_column_toggled_cb), info);

    info->show_matched_info = GTK_WIDGET(gtk_builder_get_object (builder, "show_matched_info_button"));
    g_signal_connect (G_OBJECT(info->show_matched_info), "toggled",
                      G_CALLBACK(show_matched_info_toggled_cb), info);

    info->append_text = GTK_WIDGET(gtk_builder_get_object (builder, "append_desc_notes_button"));

    // Create the checkbox, but do not show it unless there are transactions
    info->reconcile_after_close = GTK_WIDGET(gtk_builder_get_object (builder, "reconcile_after_close_button"));

    show_update = gnc_import_Settings_get_action_update_enabled (info->user_settings);
    gnc_gen_trans_init_view (info, all_from_same_account, show_update);
    heading_label = GTK_WIDGET(gtk_builder_get_object (builder, "heading_label"));
    g_assert (heading_label != NULL);

    if (heading)
        gtk_label_set_text (GTK_LABEL(heading_label), heading);

    info->transaction_processed_cb = NULL;

    /* Connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, info);

    g_object_unref (G_OBJECT(builder));
}


GNCImportMainMatcher *
gnc_gen_trans_list_new (GtkWidget *parent,
                        const gchar* heading,
                        gboolean all_from_same_account,
                        gint match_date_hardlimit,
                        gboolean show_all)
{
    GNCImportMainMatcher *info;
    GtkBuilder *builder;
    GtkWidget *box, *pbox;

    info = g_new0 (GNCImportMainMatcher, 1);

    /* Initialize the GtkDialog. */
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_dialog");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_content");

    info->main_widget = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_dialog"));
    g_assert (info->main_widget != NULL);

    /* Pack the content into the dialog vbox */
    pbox = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_vbox"));
    box = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_content"));
    gtk_box_pack_start (GTK_BOX(pbox), box, TRUE, TRUE, 0);

    /* setup the common parts */
    gnc_gen_trans_common_setup (info, parent, builder, heading,
                                all_from_same_account, match_date_hardlimit);

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(info->main_widget), GTK_WINDOW(parent));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->main_widget), GTK_WINDOW(parent));

    if (show_all)
        gtk_widget_show_all (GTK_WIDGET(info->main_widget));

    // Register this UI, it needs to be closed when the session is closed.
    info->id = gnc_register_gui_component (IMPORT_MAIN_MATCHER_CM_CLASS,
                                           NULL, /* no refresh handler */
                                           (GNCComponentCloseHandler)gnc_gen_trans_list_delete,
                                           info);
    // This ensure this dialog is closed when the session is closed.
    gnc_gui_component_set_session (info->id, gnc_get_current_session());

    info->desc_list = NULL;
    info->notes_list = NULL;
    info->memo_list = NULL;

    info->new_strings = NULL;
    return info;
}

/*****************************************************************
 *                 Assistant routines Start                      *
 *****************************************************************/

GNCImportMainMatcher *
gnc_gen_trans_assist_new (GtkWidget *parent,
                          GtkWidget *assistant_page,
                          const gchar* heading,
                          gboolean all_from_same_account,
                          gint match_date_hardlimit)
{
    GNCImportMainMatcher *info;
    GtkBuilder *builder;
    GtkWidget *box;

    info = g_new0 (GNCImportMainMatcher, 1);
    info->main_widget = GTK_WIDGET(parent);

    /* load the interface */
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_content");

    /* Pack content into Assistant page widget */
    box = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_content"));
    g_assert (box != NULL);
    gtk_box_pack_start (GTK_BOX(assistant_page), box, TRUE, TRUE, 6);

    /* setup the common parts */
    gnc_gen_trans_common_setup (info, parent, builder, heading,
                                all_from_same_account, match_date_hardlimit);

    return info;
}

void
gnc_gen_trans_assist_start (GNCImportMainMatcher *info)
{
    on_matcher_ok_clicked (NULL, info);
}

/*****************************************************************
 *                   Assistant routines End                      *
 *****************************************************************/

void
gnc_gen_trans_list_add_tp_cb (GNCImportMainMatcher *info,
                              GNCTransactionProcessedCB trans_processed_cb,
                              gpointer user_data)
{
    info->user_data = user_data;
    info->transaction_processed_cb = trans_processed_cb;
}

gboolean
gnc_gen_trans_list_run (GNCImportMainMatcher *info)
{
    gboolean result;

    /* DEBUG("Begin"); */
    result = gtk_dialog_run (GTK_DIALOG (info->main_widget));
    /* DEBUG("Result was %d", result); */

    /* No destroying here since the dialog was already destroyed through
       the ok_clicked handlers. */

    return result;
}

static const gchar*
get_required_color (const gchar *class_name)
{
    GdkRGBA color;
    static gchar *strbuf = NULL;
    GtkWidget *label = gtk_label_new ("Color");
    GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET(label));
    gtk_style_context_add_class (context, class_name);
    gnc_style_context_get_background_color (context, GTK_STATE_FLAG_NORMAL, &color);
    if (strbuf)
        g_free (strbuf);
    strbuf = gdk_rgba_to_string (&color);
    return strbuf;
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

    if (xaccTransCountSplits (sel_match->trans) == 2)
        gtk_tree_store_set (store, &child, DOWNLOADED_COL_ACCOUNT, xaccAccountGetName (
                            xaccSplitGetAccount (xaccSplitGetOtherSplit (sel_match->split))), -1);
    else
        gtk_tree_store_set (store, &child, DOWNLOADED_COL_ACCOUNT, _("-- Split Transaction --"), -1);

    ro_text = xaccPrintAmount (xaccSplitGetAmount (sel_match->split),
                               gnc_split_amount_print_info (sel_match->split, TRUE));

    gtk_tree_store_set (store, &child,
                        DOWNLOADED_COL_AMOUNT, ro_text,
                        -1);

    gtk_tree_store_set (store, &child,
                        DOWNLOADED_COL_MEMO, memo,
                        DOWNLOADED_COL_MEMO_STYLE, PANGO_STYLE_NORMAL,
                        -1);

    gtk_tree_store_set (store, &child,
                        DOWNLOADED_COL_DESCRIPTION, desc,
                        DOWNLOADED_COL_DESCRIPTION_STYLE, PANGO_STYLE_NORMAL,
                        -1);

    gtk_tree_store_set (store, &child, DOWNLOADED_COL_ENABLE, FALSE, -1);
    g_free (text);
}

static gchar *
get_peer_acct_names (Split *split)
{
    GList *names = NULL, *accounts_seen = NULL;
    gchar *retval, *name;
    for (GList *n = xaccTransGetSplitList (xaccSplitGetParent (split)); n; n = n->next)
    {
        Account *account = xaccSplitGetAccount (n->data);
        if ((n->data == split) ||
            (xaccAccountGetType (account) == ACCT_TYPE_TRADING) ||
            (g_list_find (accounts_seen, account)))
            continue;
        name = gnc_account_get_full_name (account);
        names = g_list_prepend (names, g_strdup_printf ("\"%s\"", name));
        accounts_seen = g_list_prepend (accounts_seen, account);
        g_free (name);
    }
    retval = gnc_g_list_stringjoin (names, ", ");
    g_list_free_full (names, g_free);
    g_list_free (accounts_seen);
    return retval;
}

static void
refresh_model_row (GNCImportMainMatcher *gui,
                   GtkTreeModel *model,
                   GtkTreeIter *iter,
                   GNCImportTransInfo *info)
{
    GtkTreeStore *store;
    GtkTreeSelection *selection;
    gchar *tmp, *imbalance, *text;
    const gchar *ro_text, *color = NULL;
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

    /* Notes */
    ro_text = xaccTransGetNotes (gnc_import_TransInfo_get_trans (info));
    gtk_tree_store_set (store, iter, DOWNLOADED_COL_NOTES_ORIGINAL, ro_text, -1);

    /*Description*/
    ro_text = xaccTransGetDescription (gnc_import_TransInfo_get_trans (info) );
    gtk_tree_store_set (store, iter,
                        DOWNLOADED_COL_DESCRIPTION, ro_text,
                        DOWNLOADED_COL_DESCRIPTION_ORIGINAL, ro_text,
                        -1);
    /*Memo*/
    ro_text = xaccSplitGetMemo (split);
    gtk_tree_store_set (store, iter,
                        DOWNLOADED_COL_MEMO, ro_text,
                        DOWNLOADED_COL_MEMO_ORIGINAL, ro_text,
                        -1);

    /*Actions*/

    /* Action information */
    ro_text = text = NULL;
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
                        /* Translators: %1$s is the amount to be transferred,
                           %2$s the destination account. */
                        g_strdup_printf (_("New, transfer %s to (manual) \"%s\""),
                                         imbalance, tmp);
                }
                else
                {
                    text =
                        /* Translators: %1$s is the amount to be transferred,
                           %2$s the destination account. */
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
                gchar *full_names = get_peer_acct_names (sel_match->split);
                color = get_required_color (int_not_required_class);
                if (gnc_import_TransInfo_get_match_selected_manually (info))
                {
                    text = g_strdup_printf (_("Reconcile (manual) match to %s"),
                                            full_names);
                }
                else
                {
                    text = g_strdup_printf (_("Reconcile (auto) match to %s"),
                                            full_names);
                }
                g_free (full_names);
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
                gchar *full_names = get_peer_acct_names (sel_match->split);
                color = get_required_color (int_not_required_class);
                if (gnc_import_TransInfo_get_match_selected_manually (info))
                {
                    text = g_strdup_printf (_("Update and reconcile (manual) match to %s"),
                                            full_names);
                }
                else
                {
                    text = g_strdup_printf (_("Update and reconcile (auto) match to %s"),
                                            full_names);
                }
                g_free (full_names);
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
            gtk_tree_view_column_set_visible (gui->memo_column, TRUE);

            gtk_tree_view_expand_row (GTK_TREE_VIEW(gui->view), path, TRUE);
            gtk_tree_path_free (path);
        }
    }
    selection = gtk_tree_view_get_selection (gui->view);
    gtk_tree_selection_unselect_all (selection);
}

void
gnc_gen_trans_list_add_trans (GNCImportMainMatcher *gui, Transaction *trans)
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

void
gnc_gen_trans_list_add_trans_with_ref_id (GNCImportMainMatcher *gui, Transaction *trans, guint32 ref_id)
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

/* Query the accounts used by the imported transactions to find a list of
 * candidate matching transactions.
 */
static GList*
query_imported_transaction_accounts (GNCImportMainMatcher *gui)
{
    static const int secs_per_day = 86400;
    GList* query_results = NULL;
    GList* all_accounts = NULL;
    GList* retval = NULL;
    gint match_date_limit =
        gnc_import_Settings_get_match_date_hardlimit (gui->user_settings);
    time64 min_time=G_MAXINT64, max_time=0;
    time64 match_timelimit = match_date_limit * secs_per_day;
    Query *query = qof_query_create_for (GNC_ID_SPLIT);

    /* Go through all imported transactions, gather the list of accounts, and
     * min/max date range.
     */
    for (GSList* txn = gui->temp_trans_list; txn != NULL;
         txn = g_slist_next (txn))
    {
        GNCImportTransInfo* txn_info = txn->data;
        Account *txn_account =
            xaccSplitGetAccount (gnc_import_TransInfo_get_fsplit (txn_info));
        time64 txn_time =
            xaccTransGetDate (gnc_import_TransInfo_get_trans (txn_info));
        all_accounts = g_list_prepend (all_accounts, txn_account);
        min_time = MIN(min_time, txn_time);
        max_time = MAX(max_time, txn_time);
    }

    // Make a query to find splits with the right accounts and dates.
    qof_query_set_book (query, gnc_get_current_book ());
    xaccQueryAddAccountMatch (query, all_accounts,
                              QOF_GUID_MATCH_ANY, QOF_QUERY_AND);
    xaccQueryAddDateMatchTT (query,
                             TRUE, min_time - match_timelimit,
                             TRUE, max_time + match_timelimit,
                             QOF_QUERY_AND);
    query_results = qof_query_run (query);
    g_list_free (all_accounts);
    retval = g_list_copy (query_results);
    qof_query_destroy (query);

    return retval;
}

/* Create a hash by account of all splits that could match one of the imported
 * transactions based on their account and date and organized per account.
 */
static GHashTable*
create_hash_of_potential_matches (GList *candidate_txns,
                                  GHashTable *account_hash)
{
    for (GList* candidate = candidate_txns; candidate != NULL;
         candidate = g_list_next (candidate))
    {
        Account* split_account;
        GSList* split_list;
        if (gnc_import_split_has_online_id (candidate->data))
            continue;
        split_account = xaccSplitGetAccount (candidate->data);
        /* g_hash_table_steal_extended would do the two calls in one shot but is
         * not available until GLib 2.58.
         */
        split_list = g_hash_table_lookup (account_hash, split_account);
        g_hash_table_steal (account_hash, split_account);
        split_list = g_slist_prepend (split_list, candidate->data);
        g_hash_table_insert (account_hash, split_account, split_list);
    }
    return account_hash;
}

typedef struct _match_struct
{
    GNCImportTransInfo* transaction_info;
    gint display_threshold;
    gint date_threshold;
    gint date_not_threshold;
    double fuzzy_amount;
} match_struct;

static void
match_helper (Split* data, match_struct* s)
{
    split_find_match (s->transaction_info, data,
                      s->display_threshold,
                      s->date_threshold,
                      s->date_not_threshold,
                      s->fuzzy_amount);
}

/* Iterate through the imported transactions selecting matches from the
 * potential match lists in the account hash and update the matcher with the
 * results.
 */

static void
perform_matching (GNCImportMainMatcher *gui, GHashTable *account_hash)
{
    GtkTreeModel* model = gtk_tree_view_get_model (gui->view);
    gint display_threshold =
        gnc_import_Settings_get_display_threshold (gui->user_settings);
    gint date_threshold =
        gnc_import_Settings_get_date_threshold (gui->user_settings);
    gint date_not_threshold =
        gnc_import_Settings_get_date_not_threshold (gui->user_settings);
    double fuzzy_amount =
        gnc_import_Settings_get_fuzzy_amount (gui->user_settings);

    for (GSList *imported_txn = gui->temp_trans_list; imported_txn !=NULL;
         imported_txn = g_slist_next (imported_txn))
    {
        GtkTreeIter iter;
        GNCImportMatchInfo *selected_match;
        gboolean match_selected_manually;
        GNCImportTransInfo* txn_info = imported_txn->data;
        Account *importaccount = xaccSplitGetAccount (gnc_import_TransInfo_get_fsplit (txn_info));
        match_struct s = {txn_info, display_threshold, date_threshold, date_not_threshold, fuzzy_amount};

        g_slist_foreach (g_hash_table_lookup (account_hash, importaccount),
                         (GFunc) match_helper, &s);

        // Sort the matches, select the best match, and set the action.
        gnc_import_TransInfo_init_matches (txn_info, gui->user_settings);

        selected_match = gnc_import_TransInfo_get_selected_match (txn_info);
        match_selected_manually =
            gnc_import_TransInfo_get_match_selected_manually (txn_info);

        if (selected_match)
            gnc_import_PendingMatches_add_match (gui->pending_matches,
                                                 selected_match,
                                                 match_selected_manually);

        gtk_tree_store_append (GTK_TREE_STORE (model), &iter, NULL);
        refresh_model_row (gui, model, &iter, txn_info);
    }
}

void
gnc_gen_trans_list_create_matches (GNCImportMainMatcher *gui)
{
    GHashTable* account_hash =
        g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL,
                              (GDestroyNotify)g_slist_free);
    GList *candidate_txns;
    g_assert (gui);
    candidate_txns = query_imported_transaction_accounts (gui);

    create_hash_of_potential_matches (candidate_txns, account_hash);
    perform_matching (gui, account_hash);

    g_list_free (candidate_txns);
    g_hash_table_destroy (account_hash);
    return;
}

GtkWidget *
gnc_gen_trans_list_widget (GNCImportMainMatcher *info)
{
    g_assert (info);
    return info->main_widget;
}

GtkWidget *
gnc_gen_trans_list_append_text_widget (GNCImportMainMatcher *info)
{
    g_assert (info);
    return info->append_text;
}

gboolean
query_tooltip_tree_view_cb (GtkWidget *widget, gint x, gint y,
                            gboolean keyboard_tip,
                            GtkTooltip *tooltip,
                            gpointer user_data)
{
    GtkTreeView          *tree_view = GTK_TREE_VIEW(widget);
    GtkTreeModel         *model = gtk_tree_view_get_model (tree_view);
    GtkTreePath          *path  = NULL;
    GtkTreeViewColumn    *column = NULL;
    GtkTreeIter iter;
    gboolean show_tooltip = FALSE;

    gtk_tree_view_convert_widget_to_bin_window_coords (tree_view, x, y, &x, &y);
    if (keyboard_tip || !gtk_tree_view_get_path_at_pos (tree_view, x, y, &path,
                                                        &column, NULL, NULL))
    {
        gtk_tree_path_free (path);
        return FALSE;
    }

    // Get the iter pointing to our current column
    if (gtk_tree_model_get_iter(model, &iter, path) && column)
    {
        gchar *tooltip_text = NULL;

        // Select text based on column
        gint num_col = gtk_tree_view_column_get_sort_column_id (column);
        switch (num_col)
        {
        case DOWNLOADED_COL_DESCRIPTION:
            gtk_tree_model_get (model, &iter,
                                DOWNLOADED_COL_DESCRIPTION_ORIGINAL, &tooltip_text,
                                -1);
            break;
        case DOWNLOADED_COL_MEMO:
            gtk_tree_model_get (model, &iter,
                                DOWNLOADED_COL_MEMO_ORIGINAL, &tooltip_text,
                                -1);
            break;
        default:
            break;
        }

        // Did we select any text? If yes, display the tooltip
        if (tooltip_text && *tooltip_text)
        {
            show_tooltip = TRUE;
            gtk_tooltip_set_text (tooltip, tooltip_text);
            gtk_tree_view_set_tooltip_cell (tree_view, tooltip, path, column, NULL);
        }
        g_free (tooltip_text);
    }
    // Clean up the object
    gtk_tree_path_free (path);
    return show_tooltip;
}

/** @} */
