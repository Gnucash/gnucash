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
#include <stdbool.h>

#include <memory>
#include <algorithm>
#include <vector>
#include <unordered_map>
#include <string>
#include <cstring>

#include "import-main-matcher.h"

#include "Account.hpp"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-string-utils.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-engine.h"
#include "gnc-gtk-utils.h"
#include "import-settings.h"
#include "import-backend.h"
#include "import-account-matcher.h"
#include "import-operation-teardown.h"
#include "import-pending-matches.h"
#include "gnc-component-manager.h"
#include "guid.h"
#include "gnc-session.h"
#include "Query.h"

#define GNC_PREFS_GROUP "dialogs.import.generic.transaction-list"
#define IMPORT_MAIN_MATCHER_CM_CLASS "transaction-matcher-dialog"

/* The former hierarchy view stored presentation state in model columns. GTK4 models carry
 * objects instead, so keep the transaction and its rendered state together.
 * The row object is deliberately a plain GObject: it has no business logic,
 * and the importer remains the sole owner of GNCImportTransInfo. */
struct ImportMatcherRow
{
    GNCImportTransInfo *trans_info = nullptr;
    GNCImportMatchInfo *match_info = nullptr;
    GListStore *children = nullptr;
    gchar *date = nullptr;
    gchar *account = nullptr;
    gchar *amount = nullptr;
    gchar *description = nullptr;
    gchar *description_original = nullptr;
    gchar *memo = nullptr;
    gchar *memo_original = nullptr;
    gchar *notes_original = nullptr;
    gchar *action_info = nullptr;
    gchar *color_class = nullptr;
    GdkTexture *confidence = nullptr;
    gboolean add = false;
    gboolean clear = false;
    gboolean update = false;
    gboolean enabled = false;
    gboolean detail = false;
};

static GQuark matcher_row_quark = 0;

static void
matcher_row_free (gpointer data)
{
    auto row = static_cast<ImportMatcherRow*> (data);
    if (!row)
        return;
    g_clear_object (&row->children);
    g_clear_object (&row->confidence);
    g_free (row->date);
    g_free (row->account);
    g_free (row->amount);
    g_free (row->description);
    g_free (row->description_original);
    g_free (row->memo);
    g_free (row->memo_original);
    g_free (row->notes_original);
    g_free (row->action_info);
    g_free (row->color_class);
    delete row;
}

static GObject*
matcher_row_new (GNCImportTransInfo *trans_info, gboolean detail = FALSE)
{
    if (G_UNLIKELY (!matcher_row_quark))
        matcher_row_quark = g_quark_from_static_string ("gnc-import-matcher-row");
    auto object = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    auto row = new ImportMatcherRow;
    row->trans_info = trans_info;
    row->detail = detail;
    if (!detail)
        row->children = g_list_store_new (G_TYPE_OBJECT);
    g_object_set_qdata_full (object, matcher_row_quark, row, matcher_row_free);
    return object;
}

static ImportMatcherRow*
matcher_row_get (gpointer object)
{
    return object ? static_cast<ImportMatcherRow*> (g_object_get_qdata (G_OBJECT (object), matcher_row_quark)) : nullptr;
}

struct _main_matcher_info
{
    struct MatcherLifetime *lifetime;
    GtkWidget *main_widget;
    GtkWidget *content_root;
    GtkWidget *content_parent;
    bool owns_main_window;
    GtkColumnView *view;
    GtkGestureClick *context_click;
    GtkEventControllerKey *key_controller;
    GtkPopover *context_popover; /* owned by its widget parent; weak-cleared */
    GListStore *rows;
    GtkTreeListModel *tree_model;
    GtkMultiSelection *selection;
    GNCImportSettings *user_settings;
    int selected_row;
    bool dark_theme;
    GNCTransactionProcessedCB transaction_processed_cb;
    gpointer user_data;
    GNCImportMainMatcherDoneCB done_cb;
    gpointer done_user_data;
    GncImportOperationTeardown *teardown_owner;
    GncSessionOperationContext *operation_context; /* borrowed from owner */
    GNCImportPendingMatches *pending_matches;
    GNCImportMatchPicker *match_picker;
    GtkColumnViewColumn     *account_column;
    GtkColumnViewColumn     *memo_column;
    GtkColumnViewColumn     *update_column;
    GtkWidget               *show_account_column;
    GtkWidget               *show_matched_info;
    GtkWidget               *append_text; // Update+Clear: Append import Desc/Notes to matched Desc/Notes
    GtkWidget               *reconcile_after_close;
    bool add_toggled;     // flag to indicate that add has been toggled to stop selection
    gint id;
    GSList* temp_trans_list;  // Temporary list of imported transactions
    GHashTable* acct_id_hash; // Hash table, per account, of list of transaction IDs.
    GSList* edited_accounts;  // List of accounts currently edited.

    /* only when editing fields */
    bool can_edit_desc;
    bool can_edit_notes;
    bool can_edit_memo;

    GHashTable *desc_hash;
    GHashTable *notes_hash;
    GHashTable *memo_hash;

    GList *new_strings;
    bool adjusting_selection;
};

/* Asynchronous child dialogs may outlive an embedded matcher. The assistant
 * window is deliberately not its lifetime owner: only this guard authorizes
 * a callback to touch matcher state. */
struct MatcherLifetime
{
    gatomicrefcount ref_count;
    GNCImportMainMatcher *info;
};

static MatcherLifetime *
matcher_lifetime_new (GNCImportMainMatcher *info)
{
    auto lifetime = g_new0 (MatcherLifetime, 1);
    g_atomic_ref_count_init (&lifetime->ref_count);
    lifetime->info = info;
    return lifetime;
}

static MatcherLifetime *
matcher_lifetime_ref (MatcherLifetime *lifetime)
{
    g_return_val_if_fail (lifetime, nullptr);
    g_atomic_ref_count_inc (&lifetime->ref_count);
    return lifetime;
}

static void
matcher_lifetime_unref (MatcherLifetime *lifetime)
{
    if (lifetime && g_atomic_ref_count_dec (&lifetime->ref_count))
        g_free (lifetime);
}

static GNCImportMainMatcher *
matcher_lifetime_get (MatcherLifetime *lifetime)
{
    return lifetime ? lifetime->info : nullptr;
}

static void
matcher_lifetime_invalidate (MatcherLifetime *lifetime,
                             GNCImportMainMatcher *info)
{
    if (lifetime && lifetime->info == info)
        lifetime->info = nullptr;
}

static void
matcher_lifetime_closure_destroy (gpointer data, GClosure *closure)
{
    (void)closure;
    matcher_lifetime_unref (static_cast<MatcherLifetime*> (data));
}

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

extern "C" {
void on_matcher_ok_clicked (GtkButton *button, GNCImportMainMatcher *info);
void on_matcher_cancel_clicked (GtkButton *button, gpointer user_data);
bool on_matcher_delete_event (GtkWidget *widget, gpointer data);
void on_matcher_help_clicked (GtkButton *button, gpointer user_data);
void on_matcher_help_close_clicked (GtkButton *button, gpointer user_data);
}

static void gnc_gen_trans_list_create_matches (GNCImportMainMatcher *gui);

/* Local prototypes */
static void gnc_gen_trans_assign_transfer_account (GObject *row_object,
                                                   bool *first,
                                                   bool is_selection,
                                                   Account **new_acc,
                                                   GNCImportMainMatcher *info);
static void gnc_gen_trans_assign_transfer_account_to_selection_cb (GtkButton *button,
                                                                   GNCImportMainMatcher *info);
static void gnc_gen_trans_view_popup_menu (GNCImportMainMatcher *info,
                                           GtkWidget *anchor);
static void refresh_model_row (GNCImportMainMatcher *gui,
                               GObject *row_object,
                               GNCImportTransInfo *info);
/* end local prototypes */

class GObjectUnref
{
public:
    void operator() (GObject *object) const { g_clear_object (&object); }
};

using GObjectPtr = std::unique_ptr<GObject, GObjectUnref>;

static GObjectPtr
matcher_row_at (GNCImportMainMatcher *info, guint position)
{
    auto tree_row = GTK_TREE_LIST_ROW (g_list_model_get_item (G_LIST_MODEL (info->tree_model), position));
    if (!tree_row)
        return {};
    auto item = G_OBJECT (gtk_tree_list_row_get_item (tree_row));
    g_object_unref (tree_row);
    return GObjectPtr { item };
}

static std::vector<GObjectPtr>
matcher_selected_rows (GNCImportMainMatcher *info)
{
    std::vector<GObjectPtr> selected;
    auto bitset = gtk_selection_model_get_selection (GTK_SELECTION_MODEL (info->selection));
    GtkBitsetIter iterator;
    guint position;
    for (auto valid = gtk_bitset_iter_init_first (&iterator, bitset, &position);
         valid;
         valid = gtk_bitset_iter_next (&iterator, &position))
    {
        auto row = matcher_row_at (info, position);
        if (row && !matcher_row_get (row.get ())->detail)
            selected.emplace_back (std::move (row));
    }
    gtk_bitset_unref (bitset);
    return selected;
}

static std::vector<GObjectPtr>
matcher_root_rows (GNCImportMainMatcher *info)
{
    std::vector<GObjectPtr> rows;
    auto count = g_list_model_get_n_items (G_LIST_MODEL (info->rows));
    rows.reserve (count);
    for (guint position = 0; position < count; ++position)
        rows.emplace_back (G_OBJECT (g_list_model_get_item (G_LIST_MODEL (info->rows), position)));
    return rows;
}

static GObjectPtr
matcher_find_row (GNCImportMainMatcher *info, GNCImportTransInfo *trans_info)
{
    for (auto& object : matcher_root_rows (info))
    {
        if (matcher_row_get (object.get ())->trans_info == trans_info)
            return std::move (object);
    }
    return {};
}

static void
matcher_select_object (GNCImportMainMatcher *info, GObject *row_object)
{
    auto count = g_list_model_get_n_items (G_LIST_MODEL (info->tree_model));
    for (guint position = 0; position < count; ++position)
    {
        auto object = matcher_row_at (info, position);
        if (object.get () == row_object)
        {
            gtk_selection_model_select_item (GTK_SELECTION_MODEL (info->selection),
                                             position, TRUE);
            return;
        }
    }
}

static void
matcher_row_changed (GNCImportMainMatcher *info, GObject *row_object)
{
    auto root_count = g_list_model_get_n_items (G_LIST_MODEL (info->rows));
    for (guint position = 0; position < root_count; ++position)
    {
        auto current = G_OBJECT (g_list_model_get_item (G_LIST_MODEL (info->rows), position));
        auto matches = current == row_object;
        g_object_unref (current);
        if (matches)
        {
            auto tree_row = gtk_tree_list_model_get_child_row (info->tree_model,
                                                               position);
            auto expanded = tree_row && gtk_tree_list_row_get_expanded (tree_row);
            g_clear_object (&tree_row);
            g_list_model_items_changed (G_LIST_MODEL (info->rows), position, 1, 1);
            if (expanded)
            {
                tree_row = gtk_tree_list_model_get_child_row (info->tree_model,
                                                              position);
                if (tree_row)
                    gtk_tree_list_row_set_expanded (tree_row, TRUE);
                g_clear_object (&tree_row);
            }
            return;
        }
    }
}

static void
matcher_set_all_expanded (GNCImportMainMatcher *info, gboolean expanded)
{
    auto count = g_list_model_get_n_items (G_LIST_MODEL (info->tree_model));
    for (guint position = 0; position < count; ++position)
    {
        auto tree_row = gtk_tree_list_model_get_row (info->tree_model, position);
        if (tree_row && gtk_tree_list_row_get_depth (tree_row) == 0 &&
            gtk_tree_list_row_is_expandable (tree_row))
            gtk_tree_list_row_set_expanded (tree_row, expanded);
        g_clear_object (&tree_row);
    }
}

static GListModel*
matcher_create_children (gpointer item, gpointer user_data)
{
    (void)user_data;
    auto row = matcher_row_get (item);
    if (!row || !row->children)
        return nullptr;
    return G_LIST_MODEL (g_object_ref (row->children));
}

static void
update_all_balances (GNCImportMainMatcher *info)
{
    for (GSList* iter = info->edited_accounts; iter; iter=iter->next)
    {
        auto acct = static_cast<Account*>(iter->data);
        gnc_account_set_defer_bal_computation (acct, false);
        xaccAccountRecomputeBalance (acct);
    }
    g_slist_free (info->edited_accounts);
    info->edited_accounts = NULL;
}

static void
defer_bal_computation (GNCImportMainMatcher *info, Account* acc)
{
    if (!gnc_account_get_defer_bal_computation (acc))
    {
        gnc_account_set_defer_bal_computation (acc, true);
        info->edited_accounts = g_slist_prepend (info->edited_accounts, acc);
    }
}

static void
matcher_disconnect_live_view (GNCImportMainMatcher *info)
{
    if (!info)
        return;
    if (info->view)
        g_signal_handlers_disconnect_by_data (info->view, info);
    if (info->selection)
        g_signal_handlers_disconnect_by_data (info->selection, info);
    if (info->context_click)
    {
        g_signal_handlers_disconnect_by_data (info->context_click, info);
        if (info->view)
            gtk_widget_remove_controller (GTK_WIDGET (info->view),
                                          GTK_EVENT_CONTROLLER (info->context_click));
        info->context_click = nullptr;
    }
    if (info->key_controller)
    {
        g_signal_handlers_disconnect_by_data (info->key_controller, info);
        if (info->view)
            gtk_widget_remove_controller (GTK_WIDGET (info->view),
                                          GTK_EVENT_CONTROLLER (info->key_controller));
        info->key_controller = nullptr;
    }
}

static void
matcher_clear_popover_focus (GtkPopover *popover)
{
    if (!popover)
        return;
    auto root = gtk_widget_get_root (GTK_WIDGET (popover));
    if (root)
    {
        auto focus = gtk_root_get_focus (root);
        if (focus && (focus == GTK_WIDGET (popover) ||
                      gtk_widget_is_ancestor (focus, GTK_WIDGET (popover))))
        {
            gtk_root_set_focus (root, NULL);
        }
    }
}

static void
matcher_context_popover_closed (GtkPopover *popover, MatcherLifetime *lifetime)
{
    auto info = matcher_lifetime_get (lifetime);
    if (info && info->context_popover == popover)
    {
        info->context_popover = nullptr;
        g_object_remove_weak_pointer (G_OBJECT (popover),
                                      reinterpret_cast<gpointer *> (&info->context_popover));
    }
    g_signal_handlers_disconnect_by_data (popover, lifetime);
    g_idle_add_full (G_PRIORITY_HIGH, +[](gpointer data) -> gboolean {
        auto pop = GTK_POPOVER (data);
        matcher_clear_popover_focus (pop);
        gtk_popover_set_child (pop, NULL);
        if (gtk_widget_get_parent (GTK_WIDGET (pop)))
            gtk_widget_unparent (GTK_WIDGET (pop));
        return G_SOURCE_REMOVE;
    }, g_object_ref (popover), g_object_unref);
}

static void
matcher_release_context_popover (GNCImportMainMatcher *info)
{
    if (!info || !info->context_popover)
        return;

    auto popover = info->context_popover;
    info->context_popover = nullptr;
    g_object_remove_weak_pointer (G_OBJECT (popover),
                                  reinterpret_cast<gpointer *> (&info->context_popover));
    g_signal_handlers_disconnect_by_data (popover, info->lifetime);
    matcher_clear_popover_focus (popover);
    gtk_popover_popdown (popover);
    gtk_popover_set_child (popover, NULL);
    gtk_widget_unparent (GTK_WIDGET (popover));
}

static void
matcher_detach_content (GNCImportMainMatcher *info)
{
    if (!info)
        return;

    matcher_release_context_popover (info);
    if (!info->content_root)
        return;

    /* An assistant owns its page, but the matcher owns the content appended to
     * it. Removing that root destroys the view, factories, and controllers
     * whose callbacks carry @info before the structure is released. */
    matcher_disconnect_live_view (info);
    if (info->content_parent &&
        gtk_widget_get_parent (info->content_root) == info->content_parent)
        gtk_box_remove (GTK_BOX (info->content_parent), info->content_root);
    info->content_root = nullptr;
    info->content_parent = nullptr;
}

static void
gnc_gen_trans_list_destroy (GNCImportMainMatcher *info,
                            gboolean allow_book_mutation)
{

    if (info == NULL)
        return;

    matcher_lifetime_invalidate (info->lifetime, info);
    auto match_picker = info->match_picker;
    info->match_picker = nullptr;
    if (match_picker)
    {
        gnc_import_match_picker_cancel (match_picker);
    }
    matcher_detach_content (info);

    for (auto& object : matcher_root_rows (info))
    {
        auto row = matcher_row_get (object.get ());
        if (allow_book_mutation && info->transaction_processed_cb)
            info->transaction_processed_cb (row->trans_info, false, info->user_data);
    }

    if (info->owns_main_window)
    {
        gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->main_widget));
        gnc_import_Settings_delete (info->user_settings);
        gnc_unregister_gui_component (info->id);
        gtk_window_destroy (GTK_WINDOW(info->main_widget));
        g_object_unref (info->main_widget);
    }
    else
        gnc_import_Settings_delete (info->user_settings);

    g_slist_free_full (info->temp_trans_list,
                       allow_book_mutation
                           ? (GDestroyNotify) gnc_import_TransInfo_delete
                           : (GDestroyNotify) gnc_import_TransInfo_discard);
    info->temp_trans_list = NULL;

    // We've deferred balance computations on many accounts. Let's do it now that we're done.
    if (allow_book_mutation)
        update_all_balances (info);
    else
    {
        g_slist_free (info->edited_accounts);
        info->edited_accounts = NULL;
    }

    gnc_import_PendingMatches_delete (info->pending_matches);
    g_hash_table_destroy (info->acct_id_hash);
    g_hash_table_destroy (info->desc_hash);
    g_hash_table_destroy (info->notes_hash);
    g_hash_table_destroy (info->memo_hash);
    g_list_free_full (info->new_strings, (GDestroyNotify)g_free);
    g_clear_object (&info->selection);
    g_clear_object (&info->tree_model);
    g_clear_object (&info->rows);
    gnc_import_operation_teardown_unref (info->teardown_owner);
    matcher_lifetime_unref (info->lifetime);

    g_free (info);

    if (allow_book_mutation && !gnc_gui_refresh_suspended ())
        gnc_gui_refresh_all ();
}

struct MatcherCompletion
{
    GNCImportMainMatcherDoneCB callback;
    gpointer user_data;
    gboolean accepted;
};

static MatcherCompletion
gnc_gen_trans_list_complete (GNCImportMainMatcher *info,
                             gboolean accepted,
                             gboolean mutation_allowed)
{
    MatcherCompletion completion {info->done_cb, info->done_user_data,
                                  accepted && mutation_allowed};
    info->done_cb = nullptr;
    info->done_user_data = nullptr;
    gnc_gen_trans_list_destroy (info, mutation_allowed);
    return completion;
}

static void
gnc_gen_trans_list_notify (const MatcherCompletion& completion)
{
    if (completion.callback)
        completion.callback (completion.accepted, completion.user_data);
}

static void
gnc_gen_trans_list_finish_with_operation (GNCImportMainMatcher *info,
                                          gboolean accepted,
                                          gboolean operation_held)
{
    if (!info)
        return;

    auto operation_context = info->operation_context;
    auto mutation_allowed = !operation_context;
    auto release_operation = false;
    if (operation_context)
    {
        if (!accepted && !operation_held && info->teardown_owner)
        {
            if (info->main_widget)
                gtk_widget_set_sensitive (GTK_WIDGET (info->main_widget), FALSE);
            gnc_import_operation_teardown_request (info->teardown_owner);
            return;
        }
        if (operation_held)
            mutation_allowed = gnc_session_operation_context_has_lease (
                operation_context);
        else if (accepted)
            mutation_allowed = gnc_session_operation_context_begin (
                operation_context);
        else
            mutation_allowed = gnc_session_operation_context_begin_cleanup (
                operation_context);
        release_operation = mutation_allowed;
    }

    auto completion = gnc_gen_trans_list_complete (
        info, accepted, mutation_allowed);
    if (release_operation)
        gnc_session_operation_context_end (operation_context);
    gnc_gen_trans_list_notify (completion);
}

static void
gnc_gen_trans_list_finish (GNCImportMainMatcher *info, gboolean accepted)
{
    gnc_gen_trans_list_finish_with_operation (info, accepted, FALSE);
}

void
gnc_gen_trans_list_delete (GNCImportMainMatcher *info)
{
    gnc_gen_trans_list_finish (info, FALSE);
}

void
gnc_gen_trans_list_delete_with_operation (GNCImportMainMatcher *info)
{
    if (!info || !info->operation_context)
    {
        gnc_gen_trans_list_finish (info, FALSE);
        return;
    }

    /* Transfer one recursive section to finish_with_operation, leaving the
     * caller's section owned by the caller. This keeps the matcher teardown
     * synchronous without consuming the outer OFX RAII section. */
    if (!gnc_session_operation_context_begin (info->operation_context))
    {
        gnc_gen_trans_list_finish (info, FALSE);
        return;
    }
    gnc_gen_trans_list_finish_with_operation (info, FALSE, TRUE);
}

void
gnc_gen_trans_list_delete_with_cleanup_operation (GNCImportMainMatcher *info)
{
    if (!info || !info->operation_context)
    {
        gnc_gen_trans_list_finish (info, FALSE);
        return;
    }

    /* The terminal owner must retain the outer cleanup lease until every raw
     * OFX transaction has been destroyed. The matcher consumes only this
     * explicit recursive cleanup depth. */
    g_assert (gnc_session_operation_context_has_lease (
        info->operation_context));
    g_assert (gnc_session_operation_context_begin_cleanup (
        info->operation_context));
    gnc_gen_trans_list_finish_with_operation (info, FALSE, TRUE);
}

void
gnc_gen_trans_list_discard (GNCImportMainMatcher *info)
{
    if (!info)
        return;
    auto completion = gnc_gen_trans_list_complete (info, FALSE, FALSE);
    gnc_gen_trans_list_notify (completion);
}


bool
gnc_gen_trans_list_empty (GNCImportMainMatcher *info)
{
    g_assert (info);

    // Check that both the tree model and the temporary list are empty.
    return g_list_model_get_n_items (G_LIST_MODEL (info->rows)) == 0 && !info->temp_trans_list;
}

static void
gnc_gen_trans_list_show_accounts_column (GNCImportMainMatcher *info)
{
    g_assert (info);

    auto rows = matcher_root_rows (info);
    if (rows.size () > 1)
    {
        bool multiple_accounts = false;
        auto account_name = matcher_row_get (rows.front ().get ())->account;
        for (auto& object : rows)
        {
            auto test_account_name = matcher_row_get (object.get ())->account;
            if (g_strcmp0 (account_name, test_account_name) != 0)
            {
                multiple_accounts = true;
                break;
            }
        }
        // now toggle the column
        if (multiple_accounts)
        {
            gtk_check_button_set_active (GTK_CHECK_BUTTON (info->show_account_column), true);
            matcher_set_all_expanded (info, TRUE);
        }
        else
        {
            gtk_check_button_set_active (GTK_CHECK_BUTTON (info->show_account_column), false);
            matcher_set_all_expanded (info, FALSE);
        }
    }
}

static void
resolve_conflicts (GNCImportMainMatcher *info)
{
    GList *trans_infos = nullptr;
    for (auto& object : matcher_root_rows (info))
        trans_infos = g_list_prepend (
            trans_infos, matcher_row_get (object.get ())->trans_info);
    trans_infos = g_list_reverse (trans_infos);
    gnc_import_TransInfo_resolve_conflicts (trans_infos);
    g_list_free (trans_infos);

    // Refresh all
    for (auto& object : matcher_root_rows (info))
        refresh_model_row (info, object.get (), matcher_row_get (object.get ())->trans_info);
}


static void
load_hash_tables (GNCImportMainMatcher *info)
{
    GList *accounts_list = NULL;
    for (auto& object : matcher_root_rows (info))
    {
        auto trans_info = matcher_row_get (object.get ())->trans_info;
        Split *s = gnc_import_TransInfo_get_fsplit (trans_info);
        Account *acc = xaccSplitGetAccount (s);
        if (!g_list_find (accounts_list, acc))
            accounts_list = g_list_prepend (accounts_list, acc);
    }
    for (GList *m = accounts_list; m; m = m->next)
    {
        for (auto s : xaccAccountGetSplits (static_cast<Account*>(m->data)))
        {
            const Transaction *t = xaccSplitGetParent (s);

            const gchar *key = xaccTransGetDescription (t);
            if (key && *key)
                g_hash_table_insert (info->desc_hash, (gpointer)key, one);

            key = xaccTransGetNotes (t);
            if (key && *key)
                g_hash_table_insert (info->notes_hash, (gpointer)key, one);

            key = xaccSplitGetMemo (s);
            if (key && *key)
                g_hash_table_insert (info->memo_hash, (gpointer)key, one);
        }
    }
    g_list_free (accounts_list);
}

void
gnc_gen_trans_list_show_all (GNCImportMainMatcher *info)
{
    g_assert (info);

    // Set initial state of Append checkbox to same as last import for this account.
    // Get the import account from the first split in first transaction.
    GSList *temp_trans_list = info->temp_trans_list;
    if (!temp_trans_list)
    {
        gnc_info_dialog (GTK_WINDOW (info->main_widget), "%s", _("No new transactions were found in this import."));
        return;
    }
    auto trans_info = static_cast<GNCImportTransInfo *>(temp_trans_list->data);
    Split *first_split = gnc_import_TransInfo_get_fsplit (trans_info);
    Account *account = xaccSplitGetAccount(first_split);
    gtk_check_button_set_active (GTK_CHECK_BUTTON (info->append_text),
                                 xaccAccountGetAppendText (account));

    gnc_gen_trans_list_create_matches (info);
    load_hash_tables (info);
    resolve_conflicts (info);
    gtk_widget_set_visible (GTK_WIDGET(info->main_widget), TRUE);
    gnc_gen_trans_list_show_accounts_column (info);
}

static void acc_begin_edit (GList **accounts_modified, Account *acc)
{
    if (!acc || !accounts_modified || g_list_find (*accounts_modified, acc))
        return;

    DEBUG ("xaccAccountBeginEdit for acc %s", xaccAccountGetName (acc));
    xaccAccountBeginEdit (acc);
    *accounts_modified = g_list_prepend (*accounts_modified, acc);
}
void
on_matcher_ok_clicked (GtkButton *button, GNCImportMainMatcher *info)
{
    g_assert (info);

    DEBUG ("Begin");

    auto rows = matcher_root_rows (info);
    if (rows.empty ())
    {
        // No transaction, we can just close the dialog.
        gnc_gen_trans_list_finish (info, TRUE);
        return;
    }

    if (info->operation_context &&
        !gnc_session_operation_context_begin (info->operation_context))
    {
        PWARN ("Refusing matcher commit after its import operation expired");
        gnc_gen_trans_list_finish (info, FALSE);
        return;
    }

    /* Don't run any queries and/or split sorts while processing the matcher
    results. */
    gnc_suspend_gui_refresh ();
    bool first_tran = true;
    bool append_text = gtk_check_button_get_active (
        GTK_CHECK_BUTTON (info->append_text));
    GList *accounts_modified = NULL;
    for (const auto& object : rows)
    {
        auto trans_info = matcher_row_get (object.get ())->trans_info;

        Split* first_split = gnc_import_TransInfo_get_fsplit (trans_info);
        Transaction *trans = xaccSplitGetParent (first_split);

        for (GList *n = xaccTransGetSplitList (trans); n; n = g_list_next (n))
            acc_begin_edit (&accounts_modified, xaccSplitGetAccount (static_cast<Split*>(n->data)));

        // Allow the backend to know if the Append checkbox is ticked or unticked
        // by propagating info->append_text to every trans_info->append_text
        gnc_import_TransInfo_set_append_text( trans_info, append_text);

        // When processing the first transaction,
        // save the state of the Append checkbox to an account kvp so the same state can be
        //  defaulted next time this account is imported.
        // Get the import account from the first split.
        if (first_tran)
        {
            xaccAccountSetAppendText (xaccSplitGetAccount(first_split), append_text);
            first_tran = false;
        }

        Account *dest_acc = gnc_import_TransInfo_get_destacc (trans_info);
        acc_begin_edit (&accounts_modified, dest_acc);

        if (gnc_import_process_trans_item (NULL, trans_info))
        {
            if (info->transaction_processed_cb)
            {
                info->transaction_processed_cb (trans_info, true,
                                               info->user_data);
            }
        }
    }

    DEBUG ("End");
    g_list_free_full (accounts_modified, (GDestroyNotify)xaccAccountCommitEdit);

    /* Allow GUI refresh again upon commit completion. */
    gnc_resume_gui_refresh ();
    gnc_gen_trans_list_finish_with_operation (
        info, TRUE, info->operation_context != nullptr);
}

void
on_matcher_cancel_clicked (GtkButton *button, gpointer user_data)
{
    auto info = static_cast<GNCImportMainMatcher *>(user_data);
    gnc_gen_trans_list_delete (info);
}

bool
on_matcher_delete_event (GtkWidget *widget, gpointer data)
{
    auto info = static_cast<GNCImportMainMatcher *>(data);
    (void) widget;
    gnc_gen_trans_list_delete (info);
    return true;
}

void
on_matcher_help_close_clicked (GtkButton *button, gpointer user_data)
{
    auto help_dialog = static_cast<GtkWidget *>(user_data);

    (void) button;
    gtk_window_destroy (GTK_WINDOW(help_dialog));
}

static gboolean
matcher_help_key_pressed_cb (GtkEventControllerKey *controller, guint keyval,
                             guint keycode, GdkModifierType state, gpointer user_data)
{
    GtkWidget *widget;

    (void) keycode;
    (void) state;
    (void) user_data;

    if (keyval != GDK_KEY_Escape)
        return FALSE;

    widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));
    if (GTK_IS_WINDOW (widget))
        gtk_window_destroy (GTK_WINDOW (widget));

    return TRUE;
}

void
on_matcher_help_clicked (GtkButton *button, gpointer user_data)
{
    auto info = static_cast<GNCImportMainMatcher*>(user_data);

    GtkBuilder *builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer2");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer3");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer4");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer5");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "textbuffer1");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "matcher_help_dialog");

    const gchar *class_extension = NULL;
    if (info->dark_theme == true)
        class_extension = "-dark";

    gchar *int_required_class = g_strconcat (CSS_INT_REQUIRED_CLASS, class_extension, NULL);
    gchar *int_prob_required_class = g_strconcat (CSS_INT_PROB_REQUIRED_CLASS, class_extension, NULL);
    gchar *int_not_required_class = g_strconcat (CSS_INT_NOT_REQUIRED_CLASS, class_extension, NULL);

    GtkWidget *box = GTK_WIDGET(gtk_builder_get_object (builder, "intervention_required_box"));
    gnc_widget_style_context_add_class (GTK_WIDGET(box), int_required_class);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "intervention_probably_required_box"));
    gnc_widget_style_context_add_class (GTK_WIDGET(box), int_prob_required_class);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "intervention_not_required_box"));
    gnc_widget_style_context_add_class (GTK_WIDGET(box), int_not_required_class);

    GtkWidget *help_dialog = GTK_WIDGET(gtk_builder_get_object (builder, "matcher_help_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(help_dialog), GTK_WINDOW(info->main_widget));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(help_dialog), "gnc-id-import-matcher-help");
    gnc_widget_style_context_add_class (GTK_WIDGET(help_dialog), "gnc-class-imports");

    auto close_button = GTK_WIDGET (gtk_builder_get_object (builder, "matcher_help_close"));
    auto key_controller = gtk_event_controller_key_new ();

    /* The close button is the only builder callback in this window. */
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, help_dialog);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (matcher_help_key_pressed_cb), NULL);
    gtk_widget_add_controller (help_dialog, key_controller);
    gtk_window_set_default_widget (GTK_WINDOW (help_dialog), close_button);

    g_object_unref (G_OBJECT(builder));

    g_free (int_required_class);
    g_free (int_prob_required_class);
    g_free (int_not_required_class);

    gtk_window_present (GTK_WINDOW (help_dialog));
}

struct MatchPickerCompletion
{
    MatcherLifetime *lifetime;
    GNCImportMatchPicker *picker;
};

static void
refresh_matched_transaction_cb (GNCImportTransInfo *trans_info, gpointer user_data)
{
    auto completion = static_cast<MatchPickerCompletion*> (user_data);
    auto info = matcher_lifetime_get (completion->lifetime);
    if (info)
    {
        if (info->match_picker == completion->picker)
            info->match_picker = nullptr;
        auto row = matcher_find_row (info, trans_info);
        if (row)
            refresh_model_row (info, row.get (), trans_info);
    }
    matcher_lifetime_unref (completion->lifetime);
    delete completion;
}

static void
run_match_dialog (GNCImportMainMatcher *info,
                  GNCImportTransInfo *trans_info)
{
    auto previous_picker = info->match_picker;
    info->match_picker = nullptr;
    if (previous_picker)
        gnc_import_match_picker_cancel (previous_picker);
    auto completion = new MatchPickerCompletion {
        matcher_lifetime_ref (info->lifetime), nullptr };
    auto picker = gnc_import_match_picker_run (
        info->main_widget, trans_info, info->pending_matches,
        refresh_matched_transaction_cb, completion);
    completion->picker = picker;
    if (matcher_lifetime_get (completion->lifetime) == info)
        info->match_picker = picker;
    else if (picker)
        gnc_import_match_picker_cancel (picker);
}

struct TransferAccountSelection
{
    MatcherLifetime *lifetime;
    GWeakRef matcher_window;
    std::vector<GObjectPtr> rows;
};

static void
transfer_account_selected_cb (Account *account, gboolean accepted, gpointer user_data)
{
    auto selection = static_cast<TransferAccountSelection*> (user_data);
    auto window = G_OBJECT (g_weak_ref_get (&selection->matcher_window));
    auto info = matcher_lifetime_get (selection->lifetime);
    if (window && info && accepted && account)
    {
        for (const auto& object : selection->rows)
        {
            auto row = matcher_row_get (object.get ());
            if (!row || row->detail || gnc_import_TransInfo_is_balanced (row->trans_info))
                continue;
            gnc_import_TransInfo_set_destacc (row->trans_info, account, true);
            defer_bal_computation (info, account);
            refresh_model_row (info, object.get (), row->trans_info);
        }
    }
    g_clear_object (&window);
    g_weak_ref_clear (&selection->matcher_window);
    matcher_lifetime_unref (selection->lifetime);
    delete selection;
}

static void
request_transfer_account (GNCImportMainMatcher *info, std::vector<GObjectPtr> rows)
{
    auto first = std::find_if (rows.begin (), rows.end (), [] (const auto& object)
    {
        auto row = matcher_row_get (object.get ());
        return row && !row->detail && !gnc_import_TransInfo_is_balanced (row->trans_info);
    });
    if (first == rows.end ())
        return;
    auto row = matcher_row_get (first->get ());
    auto selection = new TransferAccountSelection {
        matcher_lifetime_ref (info->lifetime), {}, std::move (rows) };
    g_weak_ref_init (&selection->matcher_window, info->main_widget);
    gnc_import_select_account_async (info->main_widget, nullptr, TRUE,
        _("Destination account for the auto-balance split."),
        xaccTransGetCurrency (gnc_import_TransInfo_get_trans (row->trans_info)),
        ACCT_TYPE_NONE, gnc_import_TransInfo_get_destacc (row->trans_info),
        transfer_account_selected_cb, selection);
}

static void
gnc_gen_trans_assign_transfer_account (GObject *row_object,
                                       bool *first,
                                       bool is_selection,
                                       Account **new_acc,
                                       GNCImportMainMatcher *info)
{
    ENTER("");
    (void)first;
    (void)new_acc;

    auto row = matcher_row_get (row_object);
    if (!row || row->detail)
        return;

    auto trans_info = row->trans_info;
    switch (gnc_import_TransInfo_get_action (trans_info))
    {
    case GNCImport_ADD:
        if (!gnc_import_TransInfo_is_balanced (trans_info))
        {
            std::vector<GObjectPtr> rows;
            rows.emplace_back (G_OBJECT (g_object_ref (row_object)));
            request_transfer_account (info, std::move (rows));
            return;
        }
        break;
    case GNCImport_CLEAR:
    case GNCImport_UPDATE:
        if (!is_selection)
            run_match_dialog (info, trans_info);
        break;
    case GNCImport_SKIP:
        break;
    default:
        PERR("InvalidGNCImportValue");
        break;
    }
    refresh_model_row (info, row_object, trans_info);
    LEAVE("");
}

static void
gnc_gen_trans_assign_transfer_account_to_selection_cb (GtkButton *button,
                                                       GNCImportMainMatcher *info)
{
    ENTER("");

    auto selected_rows = matcher_selected_rows (info);
    (void)button;

    DEBUG("Rows in selection = %zu", selected_rows.size());

    request_transfer_account (info, std::move (selected_rows));

    LEAVE("");
}

class RowInfo
{
public:
    RowInfo (GObject *object)
    {
        auto row = matcher_row_get (object);
        g_return_if_fail (row && !row->detail);
        m_trans_info = row->trans_info;
        m_orig_desc = g_strdup (row->description_original);
        m_orig_notes = g_strdup (row->notes_original);
        m_orig_memo = g_strdup (row->memo_original);
        m_object = G_OBJECT (g_object_ref (object));
    }
    ~RowInfo ()
    {
        g_free (m_orig_desc);
        g_free (m_orig_notes);
        g_free (m_orig_memo);
        g_clear_object (&m_object);
    }
    GNCImportTransInfo* get_trans_info () { return m_trans_info; };
    GObject* get_object () { return m_object; };
    const char* get_orig_desc () { return m_orig_desc; };
    const char* get_orig_notes () { return m_orig_notes; };
    const char* get_orig_memo () { return m_orig_memo; };
private:
    GNCImportTransInfo *m_trans_info;
    GObject *m_object = nullptr;
    char *m_orig_desc, *m_orig_notes, *m_orig_memo;
};

struct EntrySuggestion
{
    GtkEntry *entry;
    GtkPopover *popover;
    GtkListView *list;
    GListStore *matches;
    std::vector<std::string> candidates;
    gboolean closing;
};

struct EntryInfo
{
    GtkEntry *entry;
    GtkWidget *override_widget;
    bool can_edit;
    guint field;
    GHashTable *hash;
    const char *initial;
    EntrySuggestion suggestion;
};

struct EditFieldsDialog
{
    MatcherLifetime *lifetime;
    GtkWindow *window;
    GtkEntry *desc_entry;
    GtkEntry *notes_entry;
    GtkEntry *memo_entry;
    GtkButton *cancel_button;
    GtkButton *ok_button;
    std::vector<GObjectPtr> selected_rows;
    std::vector<EntryInfo> entries;
    gboolean finished;
};

static void
collect_entry_candidate (gpointer key, gpointer value, gpointer user_data)
{
    auto candidates = static_cast<std::vector<std::string>*> (user_data);
    auto candidate = static_cast<const char*> (key);
    (void)value;
    if (candidate && *candidate)
        candidates->emplace_back (candidate);
}

static void
suggestion_item_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto label = gtk_label_new (nullptr);
    (void)factory;
    (void)user_data;
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
}

static void
suggestion_item_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto string = GTK_STRING_OBJECT (gtk_list_item_get_item (item));
    (void)factory;
    (void)user_data;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        gtk_string_object_get_string (string));
}

static void
suggestion_activate_cb (GtkListView *view, guint position, EntrySuggestion *suggestion)
{
    if (!suggestion || suggestion->closing || !suggestion->matches ||
        !suggestion->entry || !suggestion->popover)
        return;
    auto item = GTK_STRING_OBJECT (g_list_model_get_item (G_LIST_MODEL (suggestion->matches), position));
    if (!item)
        return;
    (void)view;
    gtk_editable_set_text (GTK_EDITABLE (suggestion->entry), gtk_string_object_get_string (item));
    gtk_popover_popdown (suggestion->popover);
    g_object_unref (item);
}

static void
suggestion_update_matches (EntrySuggestion *suggestion)
{
    if (!suggestion || suggestion->closing || !suggestion->matches ||
        !suggestion->entry || !suggestion->popover)
        return;
    auto query = gtk_editable_get_text (GTK_EDITABLE (suggestion->entry));
    auto normalized = g_utf8_normalize (query, -1, G_NORMALIZE_NFC);
    auto folded_query = normalized ? g_utf8_casefold (normalized, -1) : nullptr;
    g_list_store_remove_all (suggestion->matches);

    if (folded_query && *folded_query)
    {
        for (const auto& candidate : suggestion->candidates)
        {
            auto normalized_candidate = g_utf8_normalize (candidate.c_str (), -1, G_NORMALIZE_NFC);
            auto folded_candidate = normalized_candidate ? g_utf8_casefold (normalized_candidate, -1) : nullptr;
            if (folded_candidate && strstr (folded_candidate, folded_query))
            {
                auto item = gtk_string_object_new (candidate.c_str ());
                g_list_store_append (suggestion->matches, item);
                g_object_unref (item);
            }
            g_free (folded_candidate);
            g_free (normalized_candidate);
        }
    }

    g_free (folded_query);
    g_free (normalized);
    if (g_list_model_get_n_items (G_LIST_MODEL (suggestion->matches)) > 0)
        gtk_popover_popup (suggestion->popover);
    else
        gtk_popover_popdown (suggestion->popover);
}

static void
suggestion_entry_changed_cb (GtkEditable *editable, EntrySuggestion *suggestion)
{
    (void)editable;
    suggestion_update_matches (suggestion);
}

static void
setup_entry_suggestion (EntryInfo& entryinfo)
{
    auto& suggestion = entryinfo.suggestion;
    suggestion.entry = entryinfo.entry;
    suggestion.matches = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    g_hash_table_foreach (entryinfo.hash, collect_entry_candidate, &suggestion.candidates);
    if (entryinfo.initial && *entryinfo.initial &&
        !g_hash_table_lookup (entryinfo.hash, (gpointer)entryinfo.initial))
        suggestion.candidates.emplace_back (entryinfo.initial);
    std::sort (suggestion.candidates.begin (), suggestion.candidates.end (),
               [] (const auto& left, const auto& right) { return g_utf8_collate (left.c_str (), right.c_str ()) < 0; });

    auto factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (suggestion_item_setup), nullptr);
    g_signal_connect (factory, "bind", G_CALLBACK (suggestion_item_bind), nullptr);
    /* GtkSingleSelection consumes its model reference. Keep the original for
     * the dialog's matching callbacks and explicit teardown. */
    auto selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (suggestion.matches)));
    auto list = GTK_LIST_VIEW (gtk_list_view_new (GTK_SELECTION_MODEL (selection), factory));
    suggestion.list = list;
    suggestion.popover = GTK_POPOVER (gtk_popover_new ());
    gtk_popover_set_autohide (suggestion.popover, TRUE);
    gtk_popover_set_has_arrow (suggestion.popover, FALSE);
    gtk_popover_set_child (suggestion.popover, GTK_WIDGET (list));
    gtk_widget_set_size_request (GTK_WIDGET (list), 280, -1);
    gtk_widget_set_parent (GTK_WIDGET (suggestion.popover), GTK_WIDGET (entryinfo.entry));
    g_signal_connect (list, "activate", G_CALLBACK (suggestion_activate_cb), &suggestion);
    g_signal_connect (entryinfo.entry, "changed", G_CALLBACK (suggestion_entry_changed_cb), &suggestion);
}

static void
override_widget_clicked (GtkWidget *widget, EntryInfo *entryinfo)
{
    auto dialog = static_cast<EditFieldsDialog*> (
        g_object_get_data (G_OBJECT (widget), "gnc-import-edit-fields-dialog"));
    (void)widget;
    gtk_widget_set_visible (entryinfo->override_widget, false);
    gtk_widget_set_sensitive (GTK_WIDGET (entryinfo->entry), true);
    gtk_editable_set_text (GTK_EDITABLE (entryinfo->entry), "");
    gtk_widget_grab_focus (GTK_WIDGET (entryinfo->entry));
    entryinfo->can_edit = true;
    auto info = dialog ? matcher_lifetime_get (dialog->lifetime) : nullptr;
    if (!info)
        return;
    switch (entryinfo->field)
    {
    case 0: info->can_edit_desc = true; break;
    case 1: info->can_edit_notes = true; break;
    case 2: info->can_edit_memo = true; break;
    default: break;
    }
}

static void
setup_entry (EntryInfo& entryinfo)
{
    auto sensitive = entryinfo.can_edit;
    gtk_widget_set_sensitive (GTK_WIDGET (entryinfo.entry), sensitive);
    gtk_widget_set_visible (entryinfo.override_widget, !sensitive);
    if (sensitive && entryinfo.initial && *entryinfo.initial)
        gtk_editable_set_text (GTK_EDITABLE (entryinfo.entry), entryinfo.initial);
    else if (!sensitive)
    {
        gtk_editable_set_text (GTK_EDITABLE (entryinfo.entry), _("Click Edit to modify"));
        g_signal_connect (entryinfo.override_widget, "clicked", G_CALLBACK (override_widget_clicked), &entryinfo);
    }
    setup_entry_suggestion (entryinfo);
}

static inline void
maybe_add_string (GNCImportMainMatcher *info, GHashTable *hash, const char *str)
{
    if (!str || !str[0] || g_hash_table_lookup (hash, str))
        return;
    char *new_string = g_strdup (str);
    info->new_strings = g_list_prepend (info->new_strings, new_string);
    g_hash_table_insert (hash, new_string, one);
}

static void
edit_fields_dialog_finish (EditFieldsDialog *dialog, gboolean accepted)
{
    if (!dialog || dialog->finished)
        return;
    dialog->finished = TRUE;

    /* End editable-child focus while the dialog still owns its native
     * surface, so its input context is released before widget teardown. */
    gtk_root_set_focus (GTK_ROOT (dialog->window), nullptr);
    g_signal_handlers_disconnect_by_data (dialog->window, dialog);
    if (dialog->cancel_button)
        g_signal_handlers_disconnect_by_data (dialog->cancel_button, dialog);
    if (dialog->ok_button)
        g_signal_handlers_disconnect_by_data (dialog->ok_button, dialog);

    /* The caller may retain the destroyed dialog's widgets. Quiesce every
     * closure carrying dialog-owned storage before that storage is deleted. */
    for (auto& entry : dialog->entries)
    {
        auto& suggestion = entry.suggestion;
        if (entry.override_widget)
            g_signal_handlers_disconnect_by_data (entry.override_widget, &entry);
        if (suggestion.entry)
            g_signal_handlers_disconnect_by_data (suggestion.entry, &suggestion);
        if (suggestion.list)
            g_signal_handlers_disconnect_by_data (suggestion.list, &suggestion);
    }

    auto info = matcher_lifetime_get (dialog->lifetime);
    if (accepted && info)
    {
        auto new_desc = g_strdup (gtk_editable_get_text (GTK_EDITABLE (dialog->desc_entry)));
        auto new_notes = g_strdup (gtk_editable_get_text (GTK_EDITABLE (dialog->notes_entry)));
        auto new_memo = g_strdup (gtk_editable_get_text (GTK_EDITABLE (dialog->memo_entry)));
        for (const auto& object : dialog->selected_rows)
        {
            RowInfo row { object.get () };
            auto trans = gnc_import_TransInfo_get_trans (row.get_trans_info ());
            auto split = gnc_import_TransInfo_get_fsplit (row.get_trans_info ());
            if (dialog->entries[0].can_edit)
            {
                xaccTransSetDescription (trans, new_desc);
                maybe_add_string (info, info->desc_hash, new_desc);
            }
            if (dialog->entries[1].can_edit)
            {
                xaccTransSetNotes (trans, new_notes);
                maybe_add_string (info, info->notes_hash, new_notes);
            }
            if (dialog->entries[2].can_edit)
            {
                xaccSplitSetMemo (split, new_memo);
                maybe_add_string (info, info->memo_hash, new_memo);
            }
            refresh_model_row (info, row.get_object (), row.get_trans_info ());
        }
        g_free (new_desc);
        g_free (new_notes);
        g_free (new_memo);
    }

    for (auto& entry : dialog->entries)
    {
        auto& suggestion = entry.suggestion;
        suggestion.closing = TRUE;
        auto popover = suggestion.popover;
        suggestion.popover = nullptr;
        suggestion.list = nullptr;
        suggestion.entry = nullptr;
        gtk_popover_popdown (popover);
        gtk_widget_unparent (GTK_WIDGET (popover));
        g_clear_object (&suggestion.matches);
    }
    auto window = dialog->window;
    gtk_window_destroy (window);
    g_object_unref (window);
    matcher_lifetime_unref (dialog->lifetime);
    delete dialog;
}

static void
edit_fields_button_clicked_cb (GtkButton *button, EditFieldsDialog *dialog)
{
    edit_fields_dialog_finish (dialog,
                               GPOINTER_TO_INT (g_object_get_data (G_OBJECT (button), "accepted")));
}

static gboolean
edit_fields_close_request_cb (GtkWindow *window, EditFieldsDialog *dialog)
{
    (void)window;
    edit_fields_dialog_finish (dialog, FALSE);
    return TRUE;
}

static void
input_new_fields_async (GNCImportMainMatcher *info,
                        std::vector<GObjectPtr> selected_rows)
{
    auto dialog = new EditFieldsDialog { matcher_lifetime_ref (info->lifetime), nullptr, nullptr, nullptr, nullptr,
                                         nullptr, nullptr, std::move (selected_rows), {}, FALSE };
    auto first_row = RowInfo { dialog->selected_rows[0].get () };
    auto builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_edit_dialog");

    dialog->window = GTK_WINDOW (gtk_builder_get_object (builder, "transaction_edit_dialog"));
    dialog->desc_entry = GTK_ENTRY (gtk_builder_get_object (builder, "desc_entry"));
    dialog->notes_entry = GTK_ENTRY (gtk_builder_get_object (builder, "notes_entry"));
    dialog->memo_entry = GTK_ENTRY (gtk_builder_get_object (builder, "memo_entry"));
    dialog->cancel_button = GTK_BUTTON (gtk_builder_get_object (builder, "button1"));
    dialog->ok_button = GTK_BUTTON (gtk_builder_get_object (builder, "button2"));
    g_return_if_fail (dialog->window && dialog->desc_entry && dialog->notes_entry && dialog->memo_entry &&
                      dialog->cancel_button && dialog->ok_button);
    g_object_ref (dialog->window);

    gtk_widget_set_name (GTK_WIDGET (dialog->window), "gnc-id-import-matcher-edits");
    gnc_widget_style_context_add_class (GTK_WIDGET (dialog->window), "gnc-class-imports");
    auto trans = gnc_import_TransInfo_get_trans (first_row.get_trans_info ());
    auto split = gnc_import_TransInfo_get_fsplit (first_row.get_trans_info ());
    dialog->entries.reserve (3);
    dialog->entries.push_back ({ dialog->desc_entry, GTK_WIDGET (gtk_builder_get_object (builder, "desc_override")),
                                 info->can_edit_desc, 0, info->desc_hash, xaccTransGetDescription (trans), {} });
    dialog->entries.push_back ({ dialog->notes_entry, GTK_WIDGET (gtk_builder_get_object (builder, "notes_override")),
                                 info->can_edit_notes, 1, info->notes_hash, xaccTransGetNotes (trans), {} });
    dialog->entries.push_back ({ dialog->memo_entry, GTK_WIDGET (gtk_builder_get_object (builder, "memo_override")),
                                 info->can_edit_memo, 2, info->memo_hash, xaccSplitGetMemo (split), {} });
    for (auto& entry : dialog->entries)
        g_object_set_data (G_OBJECT (entry.override_widget), "gnc-import-edit-fields-dialog", dialog);
    std::for_each (dialog->entries.begin (), dialog->entries.end (), setup_entry);

    auto focus_entry = std::find_if (dialog->entries.begin (), dialog->entries.end (),
                                     [] (const auto& entry) { return entry.can_edit; });
    if (focus_entry != dialog->entries.end ())
        gtk_widget_grab_focus (GTK_WIDGET (focus_entry->entry));
    gtk_window_set_transient_for (dialog->window, GTK_WINDOW (info->main_widget));
    gtk_window_set_modal (dialog->window, TRUE);
    gtk_window_set_default_widget (dialog->window, GTK_WIDGET (dialog->ok_button));
    g_object_set_data (G_OBJECT (dialog->cancel_button), "accepted", GINT_TO_POINTER (FALSE));
    g_object_set_data (G_OBJECT (dialog->ok_button), "accepted", GINT_TO_POINTER (TRUE));
    g_signal_connect (dialog->cancel_button, "clicked", G_CALLBACK (edit_fields_button_clicked_cb), dialog);
    g_signal_connect (dialog->ok_button, "clicked", G_CALLBACK (edit_fields_button_clicked_cb), dialog);
    g_signal_connect (dialog->window, "close-request", G_CALLBACK (edit_fields_close_request_cb), dialog);
    g_object_unref (builder);
    gtk_window_present (dialog->window);
}

struct TransferPriceSelection
{
    MatcherLifetime *lifetime;
    GWeakRef matcher_window;
    std::vector<GObjectPtr> rows;
    std::size_t index;
    gnc_numeric exch_rate;
};

static void transfer_price_selection_next (TransferPriceSelection *selection);

static void
transfer_price_selection_free (TransferPriceSelection *selection)
{
    if (!selection)
        return;
    auto window = static_cast<GtkWidget*>(g_weak_ref_get (&selection->matcher_window));
    g_clear_object (&window);
    g_weak_ref_clear (&selection->matcher_window);
    matcher_lifetime_unref (selection->lifetime);
    delete selection;
}

static void
transfer_price_selection_finished_cb (gboolean completed, gpointer user_data)
{
    auto selection = static_cast<TransferPriceSelection*>(user_data);
    auto window = static_cast<GtkWidget*>(g_weak_ref_get (&selection->matcher_window));
    auto info = matcher_lifetime_get (selection->lifetime);
    if (!completed || !window || !info)
    {
        g_clear_object (&window);
        transfer_price_selection_free (selection);
        return;
    }

    auto& object = selection->rows[selection->index];
    RowInfo row { object.get () };
    if (!gnc_numeric_zero_p (selection->exch_rate))
    {
        gnc_import_TransInfo_set_price (row.get_trans_info (),
                                        gnc_numeric_invert (selection->exch_rate));
        refresh_model_row (info, row.get_object (), row.get_trans_info ());
    }
    g_object_unref (window);
    ++selection->index;
    transfer_price_selection_next (selection);
}

static void
transfer_price_selection_next (TransferPriceSelection *selection)
{
    auto window = static_cast<GtkWidget*>(g_weak_ref_get (&selection->matcher_window));
    auto info = matcher_lifetime_get (selection->lifetime);
    if (!window || !info || selection->index >= selection->rows.size ())
    {
        g_clear_object (&window);
        transfer_price_selection_free (selection);
        return;
    }

    RowInfo row { selection->rows[selection->index].get () };
    auto trans = gnc_import_TransInfo_get_trans (row.get_trans_info ());
    auto split = gnc_import_TransInfo_get_fsplit (row.get_trans_info ());
    auto src_acc = xaccSplitGetAccount (split);
    auto dest_acc = gnc_import_TransInfo_get_destacc (row.get_trans_info ());
    auto dest_value = gnc_import_TransInfo_get_dest_value (row.get_trans_info ());
    selection->exch_rate = gnc_import_TransInfo_get_price (row.get_trans_info ());

    auto xfer = gnc_xfer_dialog (window, src_acc);
    gnc_xfer_dialog_select_to_account (xfer, dest_acc);
    gnc_xfer_dialog_set_amount (xfer, dest_value);
    gnc_xfer_dialog_set_date (xfer, xaccTransGetDate (trans));
    gnc_xfer_dialog_set_from_show_button_active (xfer, FALSE);
    gnc_xfer_dialog_set_to_show_button_active (xfer, FALSE);
    gnc_xfer_dialog_hide_from_account_tree (xfer);
    gnc_xfer_dialog_hide_to_account_tree (xfer);
    gnc_xfer_dialog_is_exchange_dialog (xfer, &selection->exch_rate);
    gnc_xfer_dialog_run_async (xfer, transfer_price_selection_finished_cb, selection);
    g_object_unref (window);
}

static void
gnc_gen_trans_set_price_to_selection_cb (GtkButton *button,
                                         GNCImportMainMatcher *info)
{
    ENTER("");
    g_return_if_fail (info);
    auto rows = matcher_selected_rows (info);
    (void)button;
    if (rows.empty ())
    {
        LEAVE ("No selected rows");
        return;
    }

    auto selection = new TransferPriceSelection {
        matcher_lifetime_ref (info->lifetime), {}, std::move (rows), 0,
        gnc_numeric_zero () };
    g_weak_ref_init (&selection->matcher_window, info->main_widget);
    transfer_price_selection_next (selection);
    LEAVE ("");
}
static void
gnc_gen_trans_edit_fields (GtkButton *button, GNCImportMainMatcher *info)
{

    ENTER("");
    g_return_if_fail (info);

    auto selected_rows = matcher_selected_rows (info);
    (void)button;

    if (selected_rows.empty())
    {
        LEAVE ("No selected rows");
        return;
    }

    input_new_fields_async (info, std::move (selected_rows));
    LEAVE("");
}

static void
gnc_gen_trans_reset_edits_cb (GtkButton *button, GNCImportMainMatcher *info)
{
    g_return_if_fail (info);
    ENTER("gnc_gen_trans_reset_edits_cb");

    auto selected_rows = matcher_selected_rows (info);
    (void)button;

    if (selected_rows.empty())
    {
        LEAVE ("No selected rows");
        return;
    }

    for (const auto& object : selected_rows)
    {
        RowInfo rowinfo { object.get () };
        auto trans = gnc_import_TransInfo_get_trans (rowinfo.get_trans_info ());
        auto split = gnc_import_TransInfo_get_fsplit (rowinfo.get_trans_info ());
        xaccTransSetDescription (trans, rowinfo.get_orig_desc());
        xaccTransSetNotes (trans, rowinfo.get_orig_notes());
        xaccSplitSetMemo (split, rowinfo.get_orig_memo());
        refresh_model_row (info, rowinfo.get_object (), rowinfo.get_trans_info ());
    };
    LEAVE("");
}

using MatcherContextAction = void (*) (GtkButton *, GNCImportMainMatcher *);

struct MatcherContextButton
{
    MatcherLifetime *lifetime;
    MatcherContextAction action;
};

static void
matcher_context_button_clicked (GtkButton *button,
                                MatcherContextButton *context)
{
    auto info = matcher_lifetime_get (context->lifetime);

    if (info)
        context->action (button, info);
}

static void
matcher_context_button_destroy (gpointer data, GClosure *closure)
{
    auto context = static_cast<MatcherContextButton *> (data);

    matcher_lifetime_unref (context->lifetime);
    g_free (context);
    (void)closure;
}

static void
gnc_gen_trans_row_activated_cb (GtkColumnView *view,
                                guint position,
                                GNCImportMainMatcher *info)
{
    ENTER("");

    bool first = true;
    bool is_selection = false;
    Account *assigned_account = NULL;
    auto object = matcher_row_at (info, position);
    (void)view;
    if (object)
    {
        gtk_selection_model_select_item (GTK_SELECTION_MODEL (info->selection), position, TRUE);
        gnc_gen_trans_assign_transfer_account (object.get (), &first, is_selection,
                                                &assigned_account, info);
    }
    LEAVE("");
}

static void
gnc_gen_trans_row_changed_cb (GtkSelectionModel *selection,
                              guint position,
                              guint n_items,
                              GNCImportMainMatcher *info)
{
    ENTER("");
    auto bitset = gtk_selection_model_get_selection (selection);
    auto selected_count = gtk_bitset_get_size (bitset);
    gtk_bitset_unref (bitset);
    if (info->adjusting_selection || selected_count < 2)
    {
        LEAVE("");
        return;
    }
    info->adjusting_selection = true;
    auto selected = matcher_selected_rows (info);
    for (auto& object : selected)
    {
        auto row = matcher_row_get (object.get ());
        if (gnc_import_TransInfo_get_action (row->trans_info) == GNCImport_ADD)
            continue;
        auto count = g_list_model_get_n_items (G_LIST_MODEL (info->tree_model));
        for (guint index = 0; index < count; ++index)
        {
            auto current = matcher_row_at (info, index);
            if (current.get () == object.get ())
                gtk_selection_model_unselect_item (selection, index);
        }
    }
    info->adjusting_selection = false;
    (void)position;
    (void)n_items;
    LEAVE("");
}

static void
gnc_gen_trans_view_popup_menu (GNCImportMainMatcher *info, GtkWidget *anchor)
{
    ENTER ("");

    matcher_release_context_popover (info);

    auto selected_rows = matcher_selected_rows (info);
    if (selected_rows.empty ())
        return;

    const char *desc = NULL, *memo = NULL, *notes = NULL;
    RowInfo first_rowinfo { selected_rows.front ().get () };
    auto first_trans = gnc_import_TransInfo_get_trans (first_rowinfo.get_trans_info ());
    auto first_split = gnc_import_TransInfo_get_fsplit (first_rowinfo.get_trans_info ());
    desc = xaccTransGetDescription (first_trans);
    notes = xaccTransGetNotes (first_trans);
    memo = xaccSplitGetMemo (first_split);

    /* determine which context menu items to enable */
    info->can_edit_desc = true;
    info->can_edit_notes = true;
    info->can_edit_memo = true;
    bool can_undo_edits = false;
    bool can_update_prices = true;
    bool can_assign_acct = true;
    for (const auto& object : selected_rows)
    {
        RowInfo rowinfo { object.get () };

        /* Only allow assigning a destination account for unbalanced transactions */
        if (can_assign_acct)
            can_assign_acct = !gnc_import_TransInfo_is_balanced (rowinfo.get_trans_info ());

        /* Only allow updating prices for transactions with a destinatin account set
         * and for which the destination account commodity is different from the
         * transaction currency */
        auto trans = gnc_import_TransInfo_get_trans (rowinfo.get_trans_info ());
        if (can_update_prices)
        {
            gnc_commodity *trans_curr = xaccTransGetCurrency (trans);
            auto dest_acc = gnc_import_TransInfo_get_destacc (rowinfo.get_trans_info ());
            if (!dest_acc || gnc_commodity_equiv (trans_curr, xaccAccountGetCommodity (dest_acc)))
                can_update_prices = false;
        }

        /* Only allow editing desc/notes/memo if they are equal for all selected
         * transactions */
        auto split = gnc_import_TransInfo_get_fsplit (rowinfo.get_trans_info ());
        if (info->can_edit_desc)
            info->can_edit_desc = !g_strcmp0 (desc, xaccTransGetDescription (trans));
        if (info->can_edit_notes)
            info->can_edit_notes = !g_strcmp0 (notes, xaccTransGetNotes (trans));
        if (info->can_edit_memo)
            info->can_edit_memo = !g_strcmp0 (memo, xaccSplitGetMemo (split));

        /* Only allow undoing desc/notes/memo edits if all selected transactions
         * have been edited */
        if (!can_undo_edits)
            can_undo_edits = (g_strcmp0 (xaccSplitGetMemo (split), rowinfo.get_orig_memo()) ||
                              g_strcmp0 (xaccTransGetNotes (trans), rowinfo.get_orig_notes()) ||
                              g_strcmp0 (xaccTransGetDescription (trans), rowinfo.get_orig_desc()));

        /* all flags were switched. no need to scan remaining rows. */
        if (!can_assign_acct && !can_update_prices &&
            !info->can_edit_desc && !info->can_edit_notes && !info->can_edit_memo &&
            can_undo_edits)
            break;
    }

    auto popover = GTK_POPOVER (gtk_popover_new ());
    auto menu = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_widget_add_css_class (menu, "menu");
    gtk_popover_set_child (popover, menu);

    auto add_menu_item = [&menu, &info](const char* name, bool sensitive,
                                        MatcherContextAction action)
    {
        auto menuitem = gtk_button_new_with_mnemonic (_(name));
        auto context = g_new0 (MatcherContextButton, 1);
        context->lifetime = matcher_lifetime_ref (info->lifetime);
        context->action = action;
        gtk_button_set_has_frame (GTK_BUTTON (menuitem), FALSE);
        gtk_widget_set_halign (menuitem, GTK_ALIGN_FILL);
        gtk_widget_set_sensitive (menuitem, sensitive);
        g_signal_connect_data (menuitem, "clicked",
                               G_CALLBACK (matcher_context_button_clicked),
                               context, matcher_context_button_destroy,
                               static_cast<GConnectFlags> (0));
        gtk_box_append (GTK_BOX (menu), menuitem);
    };

    /* Translators: Menu entry, no full stop */
    add_menu_item (N_("_Assign transfer account"),
                   can_assign_acct,
                   gnc_gen_trans_assign_transfer_account_to_selection_cb);

    /* Translators: Menu entry, no full stop */
    add_menu_item (N_("Assign e_xchange rate"),
                   can_update_prices,
                   gnc_gen_trans_set_price_to_selection_cb);

    /* Translators: Menu entry, no full stop */
    add_menu_item (N_("_Edit description, notes, or memo"),
                   info->can_edit_desc || info->can_edit_notes || info->can_edit_memo,
                   gnc_gen_trans_edit_fields);

    /* Translators: Menu entry, no full stop */
    add_menu_item (N_("_Reset all edits"),
                   can_undo_edits,
                   gnc_gen_trans_reset_edits_cb);

    gtk_widget_set_parent (GTK_WIDGET (popover), anchor ? anchor : GTK_WIDGET (info->view));
    info->context_popover = popover;
    g_object_add_weak_pointer (G_OBJECT (popover),
                               reinterpret_cast<gpointer *> (&info->context_popover));
    g_signal_connect_data (popover, "closed", G_CALLBACK (matcher_context_popover_closed),
                           matcher_lifetime_ref (info->lifetime),
                           matcher_lifetime_closure_destroy,
                           static_cast<GConnectFlags> (0));
    gtk_popover_popup (popover);
    LEAVE ("");
}

static void
gnc_gen_trans_context_pressed_cb (GtkGestureClick *gesture,
                                  gint n_press,
                                  gdouble x,
                                  gdouble y,
                                  GNCImportMainMatcher *info)
{
    (void)n_press;
    (void)x;
    (void)y;
    if (gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture)) == GDK_BUTTON_SECONDARY &&
        !matcher_selected_rows (info).empty ())
        gnc_gen_trans_view_popup_menu (info, GTK_WIDGET (info->view));
}

static gboolean
gnc_gen_trans_key_pressed_cb (GtkEventControllerKey *controller,
                              guint keyval,
                              guint keycode,
                              GdkModifierType state,
                              GNCImportMainMatcher *info)
{
    (void)controller;
    (void)keycode;
    if ((keyval == GDK_KEY_F10 && (state & GDK_SHIFT_MASK)) || keyval == GDK_KEY_Menu)
    {
        if (!matcher_selected_rows (info).empty ())
            gnc_gen_trans_view_popup_menu (info, GTK_WIDGET (info->view));
        return TRUE;
    }
    return FALSE;
}

static const gchar*
matcher_row_text (ImportMatcherRow *row, gint column)
{
    switch (column)
    {
    case DOWNLOADED_COL_DATE_TXT: return row->date;
    case DOWNLOADED_COL_ACCOUNT: return row->account;
    case DOWNLOADED_COL_AMOUNT: return row->amount;
    case DOWNLOADED_COL_DESCRIPTION: return row->description;
    case DOWNLOADED_COL_MEMO: return row->memo;
    case DOWNLOADED_COL_ACTION_INFO: return row->action_info;
    default: return nullptr;
    }
}

static void
matcher_apply_row_style (GtkWidget *widget, ImportMatcherRow *row)
{
    gtk_widget_remove_css_class (widget, CSS_INT_REQUIRED_CLASS);
    gtk_widget_remove_css_class (widget, CSS_INT_PROB_REQUIRED_CLASS);
    gtk_widget_remove_css_class (widget, CSS_INT_NOT_REQUIRED_CLASS);
    gtk_widget_remove_css_class (widget, "gnc-class-intervention-required-dark");
    gtk_widget_remove_css_class (widget, "gnc-class-intervention-probably-required-dark");
    gtk_widget_remove_css_class (widget, "gnc-class-intervention-not-required-dark");
    gtk_widget_remove_css_class (widget, "gnc-class-edited-import-field");
    if (row->color_class)
        gtk_widget_add_css_class (widget, row->color_class);
}

static void
matcher_text_setup_cb (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto column = GPOINTER_TO_INT (user_data);
    auto cell = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    auto label = gtk_label_new (nullptr);
    gtk_widget_add_css_class (cell, "gnc-import-matcher-cell");
    gtk_widget_set_hexpand (cell, TRUE);
    gtk_widget_set_valign (cell, GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (label, TRUE);
    gtk_widget_set_valign (label, GTK_ALIGN_CENTER);
    gtk_label_set_xalign (GTK_LABEL (label), column == DOWNLOADED_COL_AMOUNT ? 1.0 : 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    switch (column)
    {
    case DOWNLOADED_COL_DESCRIPTION:
        gtk_label_set_max_width_chars (GTK_LABEL (label), 36);
        break;
    case DOWNLOADED_COL_ACCOUNT:
    case DOWNLOADED_COL_MEMO:
        gtk_label_set_max_width_chars (GTK_LABEL (label), 28);
        break;
    default:
        break;
    }
    if (column == DOWNLOADED_COL_DATE_TXT)
    {
        auto expander = GTK_TREE_EXPANDER (gtk_tree_expander_new ());
        gtk_widget_set_hexpand (GTK_WIDGET (expander), TRUE);
        gtk_tree_expander_set_child (expander, label);
        gtk_box_append (GTK_BOX (cell), GTK_WIDGET (expander));
    }
    else
        gtk_box_append (GTK_BOX (cell), label);
    gtk_list_item_set_child (item, cell);
    (void)factory;
}

static void
matcher_text_bind_cb (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto column = GPOINTER_TO_INT (user_data);
    auto tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    auto object = G_OBJECT (gtk_tree_list_row_get_item (tree_row));
    auto row = matcher_row_get (object);
    auto cell = gtk_list_item_get_child (item);
    auto child = gtk_widget_get_first_child (cell);
    auto label = column == DOWNLOADED_COL_DATE_TXT
        ? GTK_LABEL (gtk_tree_expander_get_child (GTK_TREE_EXPANDER (child)))
        : GTK_LABEL (child);
    gtk_label_set_text (label, matcher_row_text (row, column));
    matcher_apply_row_style (cell, row);
    gtk_widget_remove_css_class (GTK_WIDGET (label), "gnc-class-edited-import-field");
    if (!row->detail && ((column == DOWNLOADED_COL_DESCRIPTION && g_strcmp0 (row->description, row->description_original)) ||
                         (column == DOWNLOADED_COL_MEMO && g_strcmp0 (row->memo, row->memo_original))))
        gtk_widget_add_css_class (GTK_WIDGET (label), "gnc-class-edited-import-field");
    if (column == DOWNLOADED_COL_DESCRIPTION)
        gtk_widget_set_tooltip_text (GTK_WIDGET (label), row->description_original);
    else if (column == DOWNLOADED_COL_MEMO)
        gtk_widget_set_tooltip_text (GTK_WIDGET (label), row->memo_original);
    else if (column == DOWNLOADED_COL_ACCOUNT)
        gtk_widget_set_tooltip_text (GTK_WIDGET (label), matcher_row_text (row, column));
    else
        gtk_widget_set_tooltip_text (GTK_WIDGET (label), nullptr);
    if (column == DOWNLOADED_COL_DATE_TXT)
        gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (child), tree_row);
    g_object_unref (object);
    (void)factory;
}

struct MatcherToggleBinding
{
    MatcherLifetime *lifetime;
    GNCImportAction action;
    gulong changed_id;
};

static void
matcher_toggle_binding_free (gpointer data)
{
    auto binding = static_cast<MatcherToggleBinding*> (data);
    matcher_lifetime_unref (binding->lifetime);
    delete binding;
}

static void
matcher_toggle_changed_cb (GtkCheckButton *button, MatcherToggleBinding *binding)
{
    auto object = G_OBJECT (g_object_get_data (G_OBJECT (button), "gnc-import-matcher-row"));
    auto row = matcher_row_get (object);
    if (!row || row->detail || !row->enabled)
        return;
    auto info = matcher_lifetime_get (binding->lifetime);
    if (!info)
        return;
    auto action = binding->action;
    GObjectPtr row_object { G_OBJECT (g_object_ref (object)) };
    if (gnc_import_TransInfo_get_action (row->trans_info) == action &&
        gnc_import_Settings_get_action_skip_enabled (info->user_settings))
        gnc_import_TransInfo_set_action (row->trans_info, GNCImport_SKIP);
    else
        gnc_import_TransInfo_set_action (row->trans_info, action);
    refresh_model_row (info, row_object.get (), row->trans_info);
    matcher_select_object (info, row_object.get ());
}

static void
matcher_toggle_setup_cb (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto binding = new MatcherToggleBinding {
        matcher_lifetime_ref (static_cast<MatcherLifetime*> (user_data)),
        GNCImport_ADD, 0 };
    auto button = GTK_CHECK_BUTTON (gtk_check_button_new ());
    auto cell = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_widget_add_css_class (cell, "gnc-import-matcher-cell");
    gtk_widget_add_css_class (cell, "gnc-import-matcher-action-cell");
    gtk_widget_set_hexpand (cell, TRUE);
    gtk_widget_set_valign (cell, GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (GTK_WIDGET (button), TRUE);
    gtk_widget_set_halign (GTK_WIDGET (button), GTK_ALIGN_CENTER);
    gtk_widget_set_valign (GTK_WIDGET (button), GTK_ALIGN_CENTER);
    binding->action = static_cast<GNCImportAction> (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (factory), "gnc-import-matcher-action")));
    binding->changed_id = g_signal_connect (button, "toggled", G_CALLBACK (matcher_toggle_changed_cb), binding);
    g_object_set_data_full (G_OBJECT (button), "gnc-import-matcher-toggle", binding,
                            matcher_toggle_binding_free);
    gtk_box_append (GTK_BOX (cell), GTK_WIDGET (button));
    gtk_list_item_set_child (item, cell);
}

static void
matcher_toggle_bind_cb (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto cell = gtk_list_item_get_child (item);
    auto button = GTK_CHECK_BUTTON (gtk_widget_get_first_child (cell));
    auto tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    auto object = G_OBJECT (gtk_tree_list_row_get_item (tree_row));
    auto row = matcher_row_get (object);
    auto binding = static_cast<MatcherToggleBinding*> (g_object_get_data (G_OBJECT (button), "gnc-import-matcher-toggle"));
    gboolean active = binding->action == GNCImport_ADD ? row->add :
                      binding->action == GNCImport_CLEAR ? row->clear : row->update;
    g_signal_handler_block (button, binding->changed_id);
    gtk_check_button_set_active (button, active);
    g_signal_handler_unblock (button, binding->changed_id);
    g_object_set_data_full (G_OBJECT (button), "gnc-import-matcher-row", object,
                            g_object_unref);
    gtk_widget_set_visible (GTK_WIDGET (button), row->enabled && !row->detail);
    gtk_widget_set_sensitive (GTK_WIDGET (button), row->enabled && !row->detail);
    matcher_apply_row_style (cell, row);
    (void)factory;
    (void)user_data;
}

static void
matcher_info_setup_cb (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    auto picture = gnc_import_match_score_picture_new ();
    auto label = gtk_label_new (nullptr);
    gtk_widget_add_css_class (box, "gnc-import-matcher-cell");
    gtk_widget_add_css_class (box, "gnc-import-matcher-info-cell");
    gtk_widget_set_hexpand (box, TRUE);
    gtk_widget_set_valign (box, GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (label, TRUE);
    gtk_widget_set_valign (label, GTK_ALIGN_CENTER);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_label_set_max_width_chars (GTK_LABEL (label), 36);
    gtk_box_append (GTK_BOX (box), GTK_WIDGET (picture));
    gtk_box_append (GTK_BOX (box), label);
    gtk_list_item_set_child (item, box);
    (void)factory;
    (void)user_data;
}

static void
matcher_info_bind_cb (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    auto object = G_OBJECT (gtk_tree_list_row_get_item (tree_row));
    auto row = matcher_row_get (object);
    auto box = GTK_BOX (gtk_list_item_get_child (item));
    auto picture = GTK_PICTURE (gtk_widget_get_first_child (GTK_WIDGET (box)));
    auto label = GTK_LABEL (gtk_widget_get_next_sibling (GTK_WIDGET (picture)));
    gtk_picture_set_paintable (picture, row->confidence ? GDK_PAINTABLE (row->confidence) : nullptr);
    gtk_label_set_text (label, row->action_info);
    gtk_widget_set_tooltip_text (GTK_WIDGET (label), row->action_info);
    matcher_apply_row_style (GTK_WIDGET (box), row);
    g_object_unref (object);
    (void)factory;
    (void)user_data;
}

static GtkColumnViewColumn*
matcher_add_text_column (GNCImportMainMatcher *info, const gchar *title, gint column)
{
    auto factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (matcher_text_setup_cb), GINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (matcher_text_bind_cb), GINT_TO_POINTER (column));
    auto view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    if (column == DOWNLOADED_COL_DESCRIPTION)
        gtk_column_view_column_set_expand (view_column, TRUE);
    gtk_column_view_append_column (info->view, view_column);
    g_object_unref (view_column);
    return view_column;
}

static GtkColumnViewColumn*
matcher_add_toggle_column (GNCImportMainMatcher *info, const gchar *title,
                           GNCImportAction action, const gchar *tooltip_text)
{
    auto factory = gtk_signal_list_item_factory_new ();
    g_object_set_data (G_OBJECT (factory), "gnc-import-matcher-action", GINT_TO_POINTER (action));
    g_signal_connect_data (factory, "setup", G_CALLBACK (matcher_toggle_setup_cb),
                           matcher_lifetime_ref (info->lifetime),
                           matcher_lifetime_closure_destroy,
                           static_cast<GConnectFlags> (0));
    g_signal_connect (factory, "bind", G_CALLBACK (matcher_toggle_bind_cb), nullptr);
    auto column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (column, FALSE);
    if (tooltip_text)
        g_object_set_data_full (G_OBJECT (column), "gnc-import-matcher-tooltip", g_strdup (tooltip_text), g_free);
    gtk_column_view_append_column (info->view, column);
    g_object_unref (column);
    return column;
}

static void
gnc_gen_trans_init_view (GNCImportMainMatcher *info,
                         bool show_account,
                         bool show_update)
{
    info->rows = g_list_store_new (G_TYPE_OBJECT);
    // The GTK model constructors consume the references passed to them. Keep
    // one independent reference to every model stored in info so that
    // destroying the view can't leave dangling model pointers behind.
    info->tree_model = gtk_tree_list_model_new (
        G_LIST_MODEL (g_object_ref (info->rows)), FALSE, FALSE,
        matcher_create_children, nullptr, nullptr);
    info->selection = gtk_multi_selection_new (
        G_LIST_MODEL (g_object_ref (info->tree_model)));
    info->view = GTK_COLUMN_VIEW (gtk_column_view_new (
        GTK_SELECTION_MODEL (g_object_ref (info->selection))));
    gtk_widget_add_css_class (GTK_WIDGET (info->view), "gnc-import-matcher");
    gtk_column_view_set_reorderable (info->view, TRUE);
    gtk_column_view_set_enable_rubberband (info->view, TRUE);

    matcher_add_text_column (info, _("Date"), DOWNLOADED_COL_DATE_TXT);
    info->account_column = matcher_add_text_column (info, _("Account"), DOWNLOADED_COL_ACCOUNT);
    gtk_column_view_column_set_visible (info->account_column, show_account);
    matcher_add_text_column (info, _("Amount"), DOWNLOADED_COL_AMOUNT);
    matcher_add_text_column (info, _("Description"), DOWNLOADED_COL_DESCRIPTION);
    info->memo_column = matcher_add_text_column (info, _("Memo"), DOWNLOADED_COL_MEMO);
    matcher_add_toggle_column (info, C_("Column header for 'Adding transaction'", "A"), GNCImport_ADD,
                               _("Add as a new transaction"));
    info->update_column = matcher_add_toggle_column (info,
                               C_("Column header for 'Updating plus Clearing transaction'", "U+C"), GNCImport_UPDATE,
                               _("Update + Clear Transaction\nUpdate existing transaction with the imported data and mark it as cleared"));
    gtk_column_view_column_set_visible (info->update_column, show_update);
    matcher_add_toggle_column (info, C_("Column header for 'Clearing transaction'", "C"), GNCImport_CLEAR,
                               _("Clear Transaction\nMark existing transaction as cleared without changing its details"));

    auto info_factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (info_factory, "setup", G_CALLBACK (matcher_info_setup_cb), nullptr);
    g_signal_connect (info_factory, "bind", G_CALLBACK (matcher_info_bind_cb), nullptr);
    auto info_column = gtk_column_view_column_new (_("Additional Comments"), info_factory);
    gtk_column_view_column_set_expand (info_column, TRUE);
    gtk_column_view_column_set_resizable (info_column, TRUE);
    gtk_column_view_append_column (info->view, info_column);
    g_object_unref (info_column);

    g_signal_connect (info->view, "activate", G_CALLBACK (gnc_gen_trans_row_activated_cb), info);
    g_signal_connect (info->selection, "selection-changed", G_CALLBACK (gnc_gen_trans_row_changed_cb), info);
    info->context_click = GTK_GESTURE_CLICK (gtk_gesture_click_new ());
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (info->context_click), GDK_BUTTON_SECONDARY);
    g_signal_connect (info->context_click, "pressed",
                      G_CALLBACK (gnc_gen_trans_context_pressed_cb), info);
    gtk_widget_add_controller (GTK_WIDGET (info->view),
                               GTK_EVENT_CONTROLLER (info->context_click));
    info->key_controller = GTK_EVENT_CONTROLLER_KEY (gtk_event_controller_key_new ());
    g_signal_connect (info->key_controller, "key-pressed",
                      G_CALLBACK (gnc_gen_trans_key_pressed_cb), info);
    gtk_widget_add_controller (GTK_WIDGET (info->view),
                               GTK_EVENT_CONTROLLER (info->key_controller));
}

static void
show_account_column_toggled_cb (GtkCheckButton *checkbutton,
                                GNCImportMainMatcher *info)
{
    gtk_column_view_column_set_visible (info->account_column,
        gtk_check_button_get_active (checkbutton));
}

static void
show_memo_column_toggled_cb (GtkCheckButton *checkbutton,
                             GNCImportMainMatcher *info)
{
    gtk_column_view_column_set_visible (info->memo_column,
        gtk_check_button_get_active (checkbutton));
}

static void
show_matched_info_toggled_cb (GtkCheckButton *checkbutton,
                              GNCImportMainMatcher *info)
{
    if (gtk_check_button_get_active (checkbutton))
    {
        gtk_check_button_set_active (GTK_CHECK_BUTTON (info->show_account_column), true);
        matcher_set_all_expanded (info, TRUE);
    }
    else
    {
        gtk_column_view_column_set_visible (info->account_column,
            gtk_check_button_get_active (GTK_CHECK_BUTTON (info->show_account_column)));
        matcher_set_all_expanded (info, FALSE);
    }
}

static void
gnc_gen_trans_common_setup (GNCImportMainMatcher *info,
                            GtkWidget *parent,
                            GtkBuilder *builder,
                            const gchar* heading,
                            bool all_from_same_account,
                            gint match_date_hardlimit)
{
    info->pending_matches = gnc_import_PendingMatches_new ();

    /* Initialize user Settings. */
    info->user_settings = gnc_import_Settings_new ();
    gnc_import_Settings_set_match_date_hardlimit (info->user_settings, match_date_hardlimit);


    g_assert (GTK_IS_WIDGET (info->main_widget));
    GdkRGBA color;
    gtk_widget_get_color (info->main_widget, &color);
    info->dark_theme = gnc_is_dark_theme (&color);

    /* A non-scrollable GtkScrolledWindow child is wrapped in a GtkViewport in
     * GTK4, so deriving the scrolled window from the placeholder's parent is
     * invalid. Address the stable builder object directly. */
    auto scrolled = GTK_SCROLLED_WINDOW (
        gtk_builder_get_object (builder, "scrolledwindow25"));
    g_assert (GTK_IS_SCROLLED_WINDOW (scrolled));

    info->show_account_column = GTK_WIDGET(gtk_builder_get_object (builder, "show_source_account_button"));
    g_assert (GTK_IS_CHECK_BUTTON (info->show_account_column));
    gtk_check_button_set_active (GTK_CHECK_BUTTON (info->show_account_column),
                                 all_from_same_account);
    g_signal_connect (G_OBJECT(info->show_account_column), "toggled",
                      G_CALLBACK(show_account_column_toggled_cb), info);

    auto button = GTK_CHECK_BUTTON (
        gtk_builder_get_object (builder, "show_memo_column_button"));
    g_assert (GTK_IS_CHECK_BUTTON (button));
    gtk_check_button_set_active (button, true);
    g_signal_connect (button, "toggled",
                      G_CALLBACK(show_memo_column_toggled_cb), info);

    info->show_matched_info = GTK_WIDGET(gtk_builder_get_object (builder, "show_matched_info_button"));
    g_assert (GTK_IS_CHECK_BUTTON (info->show_matched_info));
    g_signal_connect (G_OBJECT(info->show_matched_info), "toggled",
                      G_CALLBACK(show_matched_info_toggled_cb), info);

    info->append_text = GTK_WIDGET(gtk_builder_get_object (builder, "append_desc_notes_button"));
    g_assert (GTK_IS_CHECK_BUTTON (info->append_text));

    // Create the checkbox, but do not show it unless there are transactions
    info->reconcile_after_close = GTK_WIDGET(gtk_builder_get_object (builder, "reconcile_after_close_button"));
    g_assert (GTK_IS_CHECK_BUTTON (info->reconcile_after_close));


    GtkWidget *heading_label = GTK_WIDGET(gtk_builder_get_object (builder, "heading_label"));
    if (heading)
        gtk_label_set_text (GTK_LABEL(heading_label), heading);

    bool show_update = gnc_import_Settings_get_action_update_enabled (info->user_settings);
    gnc_gen_trans_init_view (info, all_from_same_account, show_update);
    gtk_scrolled_window_set_child (scrolled, GTK_WIDGET (info->view));

    info->acct_id_hash = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL,
                                                (GDestroyNotify)g_hash_table_destroy);
    info->desc_hash = g_hash_table_new (g_str_hash, g_str_equal);
    info->notes_hash = g_hash_table_new (g_str_hash, g_str_equal);
    info->memo_hash = g_hash_table_new (g_str_hash, g_str_equal);
    info->new_strings = NULL;
    info->transaction_processed_cb = NULL;
    /* Connect the signals */
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, info);

    g_object_unref (G_OBJECT(builder));
}


GNCImportMainMatcher *
gnc_gen_trans_list_new (GtkWidget *parent,
                        const gchar* heading,
                        bool all_from_same_account,
                        gint match_date_hardlimit,
                        bool show_all)
{
    GNCImportMainMatcher *info = g_new0 (GNCImportMainMatcher, 1);
    info->lifetime = matcher_lifetime_new (info);

    /* Initialize the top-level GTK4 window. */
    GtkBuilder *builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_dialog");
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_content");

    info->main_widget = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_dialog"));
    g_assert (info->main_widget != NULL);
    g_object_ref (info->main_widget);
    info->owns_main_window = true;

    /* Pack the content into the dialog vbox */
    GtkWidget *pbox = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_vbox"));
    GtkWidget *box = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_content"));
    GtkWidget *ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "matcher_ok"));
    gnc_box_append_full (GTK_BOX(pbox), box, true, true, 0);
    info->content_root = box;
    info->content_parent = pbox;

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(info->main_widget), "gnc-id-import-matcher-transactions");
    gtk_widget_set_name (GTK_WIDGET(box), "gnc-id-import-transaction-content");
    gnc_widget_style_context_add_class (GTK_WIDGET(info->main_widget), "gnc-class-imports");

    /* setup the common parts */
    gnc_gen_trans_common_setup (info, parent, builder, heading,
                                all_from_same_account, match_date_hardlimit);

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(info->main_widget), GTK_WINDOW(parent));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->main_widget), GTK_WINDOW(parent));

    gtk_window_set_default_widget (GTK_WINDOW (info->main_widget), ok_button);

    if (show_all)
        gtk_widget_set_visible (GTK_WIDGET(info->main_widget), TRUE);

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

GNCImportMainMatcher *
gnc_gen_trans_assist_new (GtkWidget *parent,
                          GtkWidget *assistant_page,
                          const gchar* heading,
                          bool all_from_same_account,
                          gint match_date_hardlimit)
{
    GNCImportMainMatcher *info = g_new0 (GNCImportMainMatcher, 1);
    info->lifetime = matcher_lifetime_new (info);
    info->main_widget = GTK_WIDGET(parent);

    /* load the interface */
    GtkBuilder *builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "transaction_matcher_content");

    /* Pack content into Assistant page widget */
    GtkWidget *box = GTK_WIDGET(gtk_builder_get_object (builder, "transaction_matcher_content"));
    g_assert (box != NULL);
    gnc_box_append_full (GTK_BOX(assistant_page), box, true, true, 6);
    info->content_root = box;
    info->content_parent = assistant_page;

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(box), "gnc-id-import-transaction-content");

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
gnc_gen_trans_list_bind_operation_teardown (
    GNCImportMainMatcher *info, GncImportOperationTeardown *owner)
{
    g_return_val_if_fail (info && owner, FALSE);
    auto context = gnc_import_operation_teardown_get_context (owner);
    if (!context || info->teardown_owner ||
        !gnc_session_operation_context_is_current (context))
        return FALSE;
    info->teardown_owner = gnc_import_operation_teardown_ref (owner);
    info->operation_context = context;
    return TRUE;
}

void
gnc_gen_trans_list_present (GNCImportMainMatcher *info,
                            GNCImportMainMatcherDoneCB done_cb,
                            gpointer user_data)
{
    g_return_if_fail (info && info->owns_main_window);
    g_return_if_fail (info->done_cb == nullptr);
    info->done_cb = done_cb;
    info->done_user_data = user_data;
    gtk_window_present (GTK_WINDOW (info->main_widget));
}

static void
remove_child_row (ImportMatcherRow *row)
{
    if (row->children)
        g_list_store_remove_all (row->children);
}

static void
update_child_row (GNCImportMatchInfo *sel_match, ImportMatcherRow *parent)
{
    auto account_str = (xaccTransCountSplits (sel_match->trans) == 2)
        ? xaccAccountGetName (xaccSplitGetAccount (xaccSplitGetOtherSplit (sel_match->split)))
        : _("-- Split Transaction --");
    auto amount_str = xaccPrintAmount (xaccSplitGetAmount (sel_match->split), gnc_split_amount_print_info (sel_match->split, true));
    auto date = qof_print_date (xaccTransGetDate (sel_match->trans));
    g_list_store_remove_all (parent->children);
    auto object = matcher_row_new (nullptr, TRUE);
    auto child = matcher_row_get (object);
    child->match_info = sel_match;
    child->account = g_strdup (account_str);
    child->date = date;
    child->amount = g_strdup (amount_str);
    child->memo = g_strdup (xaccSplitGetMemo (sel_match->split));
    child->description = g_strdup (xaccTransGetDescription (sel_match->trans));
    child->enabled = FALSE;
    g_list_store_append (parent->children, object);
    g_object_unref (object);
}

static gchar *
get_peer_acct_names (Split *split)
{
    GList *names = NULL, *accounts_seen = NULL;
    for (GList *n = xaccTransGetSplitList (xaccSplitGetParent (split)); n; n = n->next)
    {
        Account *account = xaccSplitGetAccount (static_cast<Split*>(n->data));
        if ((n->data == split) ||
            (xaccAccountGetType (account) == ACCT_TYPE_TRADING) ||
            (g_list_find (accounts_seen, account)))
            continue;
        gchar *name = gnc_account_get_full_name (account);
        names = g_list_prepend (names, name);
        accounts_seen = g_list_prepend (accounts_seen, account);
    }
    names = g_list_sort (names, (GCompareFunc)g_utf8_collate);
    auto retval = gnc_list_formatter (names);
    g_list_free_full (names, g_free);
    g_list_free (accounts_seen);
    return retval;
}

static void
refresh_model_row (GNCImportMainMatcher *gui,
                   GObject *row_object,
                   GNCImportTransInfo *info)
{
    g_assert (gui && row_object && info);
    auto row = matcher_row_get (row_object);
    g_assert (row && !row->detail);
    auto replace = [] (gchar **target, const gchar *value)
    {
        g_free (*target);
        *target = g_strdup (value);
    };

    row->trans_info = info;
    row->enabled = TRUE;
    Split *split = gnc_import_TransInfo_get_fsplit (info);
    g_assert (split);
    replace (&row->account, xaccAccountGetName (xaccSplitGetAccount (split)));
    auto date = qof_print_date (xaccTransGetDate (gnc_import_TransInfo_get_trans (info)));
    replace (&row->date, date);
    g_free (date);
    auto amount = xaccPrintAmount (xaccSplitGetAmount (split), gnc_split_amount_print_info (split, true));
    replace (&row->amount, amount);

    auto description = xaccTransGetDescription (gnc_import_TransInfo_get_trans (info));
    auto notes = xaccTransGetNotes (gnc_import_TransInfo_get_trans (info));
    auto memo = xaccSplitGetMemo (split);
    if (!row->description_original)
        replace (&row->description_original, description);
    if (!row->notes_original)
        replace (&row->notes_original, notes);
    if (!row->memo_original)
        replace (&row->memo_original, memo);
    replace (&row->description, description);
    replace (&row->memo, memo);

    const gchar *class_extension = gui->dark_theme ? "-dark" : "";
    auto required = g_strconcat (CSS_INT_REQUIRED_CLASS, class_extension, nullptr);
    auto probably_required = g_strconcat (CSS_INT_PROB_REQUIRED_CLASS, class_extension, nullptr);
    auto not_required = g_strconcat (CSS_INT_NOT_REQUIRED_CLASS, class_extension, nullptr);
    const gchar *class_name = nullptr;
    gchar *text = nullptr;
    gboolean show_confidence = TRUE;
    auto selected_match = gnc_import_TransInfo_get_selected_match (info);

    switch (gnc_import_TransInfo_get_action (info))
    {
    case GNCImport_ADD:
        if (gnc_import_TransInfo_is_balanced (info))
        {
            text = g_strdup (_("New, already balanced"));
            class_name = not_required;
        }
        else
        {
            auto dest_acc = gnc_import_TransInfo_get_destacc (info);
            GNCPrintAmountInfo pinfo;
            gchar *imbalance;
            if (dest_acc)
            {
                auto dest_name = gnc_account_get_full_name (dest_acc);
                auto dest_amount = gnc_import_TransInfo_get_dest_amount (info);
                if (!gnc_numeric_zero_p (dest_amount))
                {
                    pinfo = gnc_commodity_print_info (xaccAccountGetCommodity (dest_acc), true);
                    imbalance = g_strdup (xaccPrintAmount (dest_amount, pinfo));
                    text = g_strdup_printf (gnc_import_TransInfo_get_destacc_selected_manually (info)
                                            ? _("New, transfer %s to (manual) \"%s\"")
                                            : _("New, transfer %s to (auto) \"%s\""), imbalance, dest_name);
                    class_name = not_required;
                }
                else
                {
                    pinfo = gnc_commodity_print_info (xaccTransGetCurrency (gnc_import_TransInfo_get_trans (info)), true);
                    imbalance = g_strdup (xaccPrintAmount (gnc_import_TransInfo_get_dest_value (info), pinfo));
                    text = g_strdup_printf (_("New, UNBALANCED (need price to transfer %s to acct %s)!"), imbalance, dest_name);
                    class_name = required;
                }
                g_free (dest_name);
            }
            else
            {
                pinfo = gnc_commodity_print_info (xaccTransGetCurrency (gnc_import_TransInfo_get_trans (info)), true);
                imbalance = g_strdup (xaccPrintAmount (gnc_import_TransInfo_get_dest_value (info), pinfo));
                text = g_strdup_printf (_("New, UNBALANCED (need acct to transfer %s)!"), imbalance);
                class_name = probably_required;
            }
            g_free (imbalance);
            remove_child_row (row);
        }
        break;
    case GNCImport_CLEAR:
    case GNCImport_UPDATE:
        if (selected_match)
        {
            auto names = get_peer_acct_names (selected_match->split);
            gboolean manual = gnc_import_TransInfo_get_match_selected_manually (info);
            if (gnc_import_TransInfo_get_action (info) == GNCImport_CLEAR)
                text = g_strdup_printf (manual ? _("Reconcile (manual) match to %s") : _("Reconcile (auto) match to %s"), names);
            else
                text = g_strdup_printf (manual ? _("Update and reconcile (manual) match to %s") : _("Update and reconcile (auto) match to %s"), names);
            class_name = not_required;
            g_free (names);
            update_child_row (selected_match, row);
        }
        else
        {
            text = g_strdup (_("Match missing!"));
            class_name = required;
            show_confidence = FALSE;
            remove_child_row (row);
        }
        break;
    case GNCImport_SKIP:
        text = g_strdup (_("Do not import (no action selected)"));
        class_name = required;
        show_confidence = FALSE;
        remove_child_row (row);
        break;
    default:
        text = g_strdup ("WRITEME, this is an unknown action");
        class_name = required;
        show_confidence = FALSE;
        break;
    }

    replace (&row->action_info, text);
    replace (&row->color_class, class_name);
    row->add = gnc_import_TransInfo_get_action (info) == GNCImport_ADD;
    row->clear = gnc_import_TransInfo_get_action (info) == GNCImport_CLEAR;
    row->update = gnc_import_TransInfo_get_action (info) == GNCImport_UPDATE;
    g_clear_object (&row->confidence);
    if (show_confidence && selected_match && (row->clear || row->update))
    {
        auto pixbuf = gen_probability_pixbuf (gnc_import_MatchInfo_get_probability (selected_match),
                                              gui->user_settings, GTK_WIDGET (gui->view));
        if (pixbuf)
        {
            row->confidence = gnc_texture_new_from_pixbuf (pixbuf);
            g_object_unref (pixbuf);
        }
    }
    if (row->children && gtk_check_button_get_active (
            GTK_CHECK_BUTTON (gui->show_matched_info)))
    {
        gtk_column_view_column_set_visible (gui->account_column, TRUE);
        gtk_column_view_column_set_visible (gui->memo_column, TRUE);
    }
    gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (gui->selection));
    matcher_row_changed (gui, row_object);
    if (row->children && gtk_check_button_get_active (
            GTK_CHECK_BUTTON (gui->show_matched_info)))
        matcher_set_all_expanded (gui, TRUE);
    g_free (required);
    g_free (probably_required);
    g_free (not_required);
    g_free (text);
}

void
gnc_gen_trans_list_show_reconcile_after_close_button (GNCImportMainMatcher *info,
                                                      bool reconcile_after_close,
                                                      bool active)
{
    gtk_widget_set_visible (info->reconcile_after_close, reconcile_after_close);
    gtk_check_button_set_active (GTK_CHECK_BUTTON (info->reconcile_after_close),
                                 active);
}

GtkWidget*
gnc_gen_trans_list_get_reconcile_after_close_button (GNCImportMainMatcher *info)
{
    return info->reconcile_after_close;
}


static void
gnc_gen_trans_list_add_trans_internal (GNCImportMainMatcher *gui, Transaction *trans,
                                       guint32 ref_id, GNCImportLastSplitInfo* lsplit)
{
    g_assert (gui);
    g_assert (trans);

    Split *split = xaccTransGetSplit (trans, 0);
    Account *acc = xaccSplitGetAccount (split);
    defer_bal_computation (gui, acc);

    if (gnc_import_exists_online_id (trans, gui->acct_id_hash))
    {
        /* If it does, abort the process for this transaction, since
           it is already in the system. */
        DEBUG("Transaction with online ID exists, destroying current transaction");
        xaccTransDestroy(trans);
        xaccTransCommitEdit(trans);
        return;
    }

    GNCImportTransInfo *transaction_info = gnc_import_TransInfo_new (trans, NULL);
    gnc_import_TransInfo_set_ref_id (transaction_info, ref_id);
    gnc_import_TransInfo_set_last_split_info (transaction_info, lsplit);
    // It's much faster to gather the imported transactions into a GSList than
    // directly into the treeview.
    gui->temp_trans_list = g_slist_prepend (gui->temp_trans_list, transaction_info);
}

void
gnc_gen_trans_list_add_trans (GNCImportMainMatcher *gui, Transaction *trans)
{
    gnc_gen_trans_list_add_trans_internal (gui, trans, 0, NULL);
}

gboolean
gnc_gen_trans_list_add_trans_with_operation (GNCImportMainMatcher *gui,
                                              Transaction *trans)
{
    g_return_val_if_fail (gui && trans, FALSE);
    if (!gui->operation_context ||
        !gnc_session_operation_context_has_lease (gui->operation_context))
        return FALSE;
    gnc_gen_trans_list_add_trans_internal (gui, trans, 0, NULL);
    return TRUE;
}

void
gnc_gen_trans_list_add_trans_with_ref_id (GNCImportMainMatcher *gui, Transaction *trans, guint32 ref_id)
{
    gnc_gen_trans_list_add_trans_internal (gui, trans, ref_id, NULL);
}

void gnc_gen_trans_list_add_trans_with_split_data (GNCImportMainMatcher *gui,
                                                   Transaction *trans,
                                                   GNCImportLastSplitInfo *lsplit)
{
    gnc_gen_trans_list_add_trans_internal (gui, trans, 0, lsplit);
}

/* Return a list of splits from already existing transactions for
 * which the account matches an account used by the transactions to
 * import. The matching range is also date-limited (configurable
 * via preferences) to not go too far in the past or future.
 */
static GList*
filter_existing_splits_on_account_and_date (GNCImportMainMatcher *gui)
{
    static const int secs_per_day = 86400;
    gint match_date_limit =
        gnc_import_Settings_get_match_date_hardlimit (gui->user_settings);
    time64 min_time=G_MAXINT64, max_time=0;
    time64 match_timelimit = match_date_limit * secs_per_day;
    GList *all_accounts = NULL;

    /* Go through all imported transactions, gather the list of accounts, and
     * min/max date range.
     */
    for (GSList* txn = gui->temp_trans_list; txn != NULL;
         txn = g_slist_next (txn))
    {
        auto txn_info = static_cast<GNCImportTransInfo*>(txn->data);
        Account *txn_account =
            xaccSplitGetAccount (gnc_import_TransInfo_get_fsplit (txn_info));
        time64 txn_time =
            xaccTransGetDate (gnc_import_TransInfo_get_trans (txn_info));
        all_accounts = g_list_prepend (all_accounts, txn_account);
        min_time = MIN(min_time, txn_time);
        max_time = MAX(max_time, txn_time);
    }

    // Make a query to find splits with the right accounts and dates.
    Query *query = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (query, gnc_get_current_book ());
    xaccQueryAddAccountMatch (query, all_accounts,
                              QOF_GUID_MATCH_ANY, QOF_QUERY_AND);
    xaccQueryAddDateMatchTT (query,
                             true, min_time - match_timelimit,
                             true, max_time + match_timelimit,
                             QOF_QUERY_AND);
    GList *query_results = qof_query_run (query);
    g_list_free (all_accounts);
    GList *retval = g_list_copy (query_results);
    qof_query_destroy (query);

    return retval;
}

/* Create a hash by account of all splits that could match one of the imported
 * transactions based on their account and date and organized per account.
 */
static GHashTable*
create_hash_of_potential_matches (GList *candidate_splits,
                                  GHashTable *account_hash)
{
    for (GList* candidate = candidate_splits; candidate != NULL;
         candidate = g_list_next (candidate))
    {
        auto split = static_cast<Split*>(candidate->data);
        if (xaccSplitHasOnlineID (split))
            continue;
        /* In this context an open transaction represents a freshly
         * downloaded one. That can't possibly be a match yet */
        if (xaccTransIsOpen(xaccSplitGetParent(split)))
            continue;
        Account *split_account = xaccSplitGetAccount (split);
        /* g_hash_table_steal_extended would do the two calls in one shot but is
         * not available until GLib 2.58.
         */
        auto split_list = static_cast<GSList*>(g_hash_table_lookup (account_hash, split_account));
        g_hash_table_steal (account_hash, split_account);
        split_list = g_slist_prepend (split_list, split);
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
    gint display_threshold =
        gnc_import_Settings_get_display_threshold (gui->user_settings);
    gint date_threshold =
        gnc_import_Settings_get_date_threshold (gui->user_settings);
    gint date_not_threshold =
        gnc_import_Settings_get_date_not_threshold (gui->user_settings);
    double fuzzy_amount =
        gnc_import_Settings_get_fuzzy_amount (gui->user_settings);

    std::vector<GNCImportTransInfo*> transactions;
    for (GSList *imported_txn = gui->temp_trans_list; imported_txn !=NULL;
         imported_txn = g_slist_next (imported_txn))
    {
        auto txn_info = static_cast<GNCImportTransInfo*>(imported_txn->data);
        Account *importaccount = xaccSplitGetAccount (gnc_import_TransInfo_get_fsplit (txn_info));
        match_struct s = {txn_info, display_threshold, date_threshold, date_not_threshold, fuzzy_amount};

        g_slist_foreach (static_cast<GSList*>(g_hash_table_lookup (account_hash, importaccount)),
                         (GFunc) match_helper, &s);

        // Sort the matches, select the best match, and set the action.
        gnc_import_TransInfo_init_matches (txn_info, gui->user_settings);

        GNCImportMatchInfo *selected_match = gnc_import_TransInfo_get_selected_match (txn_info);
        bool match_selected_manually =
            gnc_import_TransInfo_get_match_selected_manually (txn_info);

        if (selected_match)
            gnc_import_PendingMatches_add_match (gui->pending_matches,
                                                 selected_match,
                                                 match_selected_manually);
        transactions.emplace_back (txn_info);
    }
    std::sort (transactions.begin (), transactions.end (), [] (auto left, auto right)
    {
        return xaccTransGetDate (gnc_import_TransInfo_get_trans (left)) <
               xaccTransGetDate (gnc_import_TransInfo_get_trans (right));
    });
    for (auto txn_info : transactions)
    {
        auto object = matcher_row_new (txn_info);
        refresh_model_row (gui, object, txn_info);
        g_list_store_append (gui->rows, object);
        g_object_unref (object);
    }
}

void
gnc_gen_trans_list_create_matches (GNCImportMainMatcher *gui)
{
    GHashTable* account_hash =
        g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL,
                              (GDestroyNotify)g_slist_free);
    g_assert (gui);
    GList *candidate_splits = filter_existing_splits_on_account_and_date (gui);

    create_hash_of_potential_matches (candidate_splits, account_hash);
    perform_matching (gui, account_hash);

    g_list_free (candidate_splits);
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

/** @} */
