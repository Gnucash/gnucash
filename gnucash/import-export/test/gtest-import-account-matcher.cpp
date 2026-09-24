/********************************************************************
 * gtest-import-account-matcher.cpp --                              *
 *                        unit tests import-account-matcher.        *
 * Copyright (C) 2020 John Ralls <jralls@ceridwen.us>               *
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
 *                                                                  *
 *******************************************************************/

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include <config.h>
#include <import-account-matcher.h>
#include <import-backend.h>
#include <import-main-matcher.h>
#include <import-match-picker.h>
#include <import-operation-teardown.h>
#include <import-pending-matches.h>
#include <gnc-amount-edit.h>
#include <gnc-ofx-import-teardown.h>
#include <gnc-prefs.h>
#include <gnc-prefs-utils.h>
#include <gnc-session.h>
#include <gnc-ui-util.h>
#include <gnc-commodity.h>
#include <gnc-engine.h>
#include <qofbook.h>
#include <Account.h>
#include <Transaction.h>
#include <gtk/gtk.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iterator>
#include <memory>
#include <string>
#include <vector>

using AccountV = std::vector<const Account*>;
using AccountTypeV = std::vector<GNCAccountType>;
using AccountPair = std::pair<AccountV&,
                              const AccountTypeV&>;

class ImportMatcherTest : public ::testing::Test
{
protected:
    static void SetUpTestSuite ()
    {
        gtk_init ();
        ASSERT_TRUE (gtk_is_initialized ());
        auto display = gdk_display_get_default ();
        ASSERT_NE (display, nullptr);
        for (const auto *name : {"gnucash-fallback.css", "gnucash.css"})
        {
            auto provider = gtk_css_provider_new ();
            auto path = g_build_filename (GNC_IMPORT_MATCHER_TEST_SRCDIR,
                                          "gnucash", name, nullptr);
            ASSERT_TRUE (g_file_test (path, G_FILE_TEST_IS_REGULAR));
            gboolean parse_error = FALSE;
            g_signal_connect (provider, "parsing-error",
                              G_CALLBACK (+[](GtkCssProvider*, GtkCssSection*,
                                               const GError*, gpointer data) {
                                  *static_cast<gboolean*> (data) = TRUE;
                              }), &parse_error);
            gtk_css_provider_load_from_path (provider, path);
            ASSERT_FALSE (parse_error) << path;
            gtk_style_context_add_provider_for_display (
                display, GTK_STYLE_PROVIDER (provider),
                g_str_has_suffix (name, "fallback.css")
                    ? GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
                    : GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
            g_object_unref (provider);
            g_free (path);
        }
        gnc_engine_init_static (0, nullptr);
        gnc_prefs_init ();
        g_log_set_always_fatal (static_cast<GLogLevelFlags> (
            G_LOG_FATAL_MASK | G_LOG_LEVEL_CRITICAL));
    }

    static void TearDownTestSuite ()
    {
        gnc_prefs_remove_registered ();
        gnc_engine_shutdown ();
    }

    ImportMatcherTest() :
        m_book{gnc_get_current_book()},
        m_root{gnc_book_get_root_account(m_book)},
        m_currency{gnc_commodity_table_lookup (
            gnc_commodity_table_get_table (m_book),
            GNC_COMMODITY_NS_CURRENCY, "USD")}
    {
        g_assert_nonnull (m_currency);
        auto create_account = [this](Account* parent, GNCAccountType type,
                                     const char* name,
                                     const char* online)->Account* {
            auto account = xaccMallocAccount(this->m_book);
            xaccAccountBeginEdit(account);
            xaccAccountSetType(account, type);
            xaccAccountSetName(account, name);
            xaccAccountSetCommodity(account, m_currency);
            xaccAccountBeginEdit(parent);
            gnc_account_append_child(parent, account);
            if (online)
                qof_instance_set(QOF_INSTANCE(account), "online-id", online, NULL);
            xaccAccountCommitEdit(parent);
            xaccAccountCommitEdit(account);
            return account;
        };
        m_assets = create_account(m_root, ACCT_TYPE_ASSET,
                                  "Assets", nullptr);
        m_expenses = create_account(m_root, ACCT_TYPE_EXPENSE,
                                    "Expenses", nullptr);
        m_bank = create_account(m_assets, ACCT_TYPE_BANK, "Bank", "Bank");
        auto broker = create_account(m_assets, ACCT_TYPE_ASSET,
                                     "Broker", "Broker");
        auto stocks = create_account(broker, ACCT_TYPE_STOCK,
                                     "Stocks", "BrokerStocks");
        create_account(stocks, ACCT_TYPE_STOCK, "AAPL", "BrokerStocksAAPL");
        create_account(stocks, ACCT_TYPE_STOCK, "MSFT", "BrokerStocksMSFT ");
        create_account(stocks, ACCT_TYPE_STOCK, "HPE", "BrokerStocksHPE");
        create_account(broker, ACCT_TYPE_BANK, "Cash Management",
                       "BrokerCash Management");
       create_account(m_expenses, ACCT_TYPE_EXPENSE, "Food", nullptr);
        create_account(m_expenses, ACCT_TYPE_EXPENSE, "Gas", nullptr);
        create_account(m_expenses, ACCT_TYPE_EXPENSE, "Rent", nullptr);
   }
    ~ImportMatcherTest()
    {
        gnc_clear_current_session();
    }

    QofBook* m_book;
    Account* m_root;
    gnc_commodity* m_currency;
    Account* m_assets;
    Account* m_bank;
    Account* m_expenses;
};

struct AccountSelectionResult
{
    Account *account {nullptr};
    gboolean accepted {FALSE};
    guint calls {0};
};

static guint
account_picker_position (GtkSingleSelection *selection, Account *account)
{
    auto rows = gtk_single_selection_get_model (selection);
    auto row_quark = g_quark_from_static_string ("gnc-import-account-picker-row");

    for (guint position = 0;
         position < g_list_model_get_n_items (rows); ++position)
    {
        auto row = G_OBJECT (g_list_model_get_item (rows, position));
        auto row_account = static_cast<Account*> (g_object_get_qdata (row, row_quark));
        g_object_unref (row);
        if (row_account == account)
            return position;
    }
    return GTK_INVALID_LIST_POSITION;
}

static void
account_selected (Account *account, gboolean accepted, gpointer user_data)
{
    auto result = static_cast<AccountSelectionResult*> (user_data);

    result->account = account;
    result->accepted = accepted;
    result->calls++;
}

static GtkWidget *
find_buildable_widget (GtkWidget *widget, const gchar *buildable_id)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_buildable_id (GTK_BUILDABLE (widget)),
                   buildable_id) == 0)
        return widget;

    for (auto child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        auto result = find_buildable_widget (child, buildable_id);

        if (result)
            return result;
    }
    return nullptr;
}

static GtkWindow *
find_buildable_window (const gchar *buildable_id)
{
    auto windows = gtk_window_get_toplevels ();

    for (guint position = 0;
         position < g_list_model_get_n_items (windows); position++)
    {
        auto window = GTK_WINDOW (g_list_model_get_item (windows, position));

        if (g_strcmp0 (gtk_buildable_get_buildable_id (GTK_BUILDABLE (window)),
                       buildable_id) == 0)
            return window;
        g_object_unref (window);
    }
    return nullptr;
}

static guint
count_buildable_windows (const gchar *buildable_id)
{
    auto windows = gtk_window_get_toplevels ();
    guint count = 0;

    for (guint position = 0;
         position < g_list_model_get_n_items (windows); ++position)
    {
        auto window = GTK_WINDOW (g_list_model_get_item (windows, position));
        if (g_strcmp0 (gtk_buildable_get_buildable_id (GTK_BUILDABLE (window)),
                       buildable_id) == 0)
            ++count;
        g_object_unref (window);
    }
    return count;
}

static bool
wait_until_buildable_window_closed (const gchar *buildable_id)
{
    const auto deadline = g_get_monotonic_time () + 2 * G_TIME_SPAN_SECOND;

    do
    {
        while (g_main_context_pending (nullptr))
            g_main_context_iteration (nullptr, FALSE);
        if (count_buildable_windows (buildable_id) == 0)
            return true;
        g_usleep (1000);
    }
    while (g_get_monotonic_time () < deadline);
    return false;
}

static GtkWidget *
find_button_with_label (GtkWidget *widget, const char *label)
{
    if (GTK_IS_BUTTON (widget) &&
        g_strcmp0 (gtk_button_get_label (GTK_BUTTON (widget)), label) == 0)
        return widget;
    for (auto child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        if (auto result = find_button_with_label (child, label))
            return result;
    }
    return nullptr;
}

static gboolean
open_matcher_context_menu (GtkColumnView *view)
{
    auto model = gtk_column_view_get_model (view);
    if (!GTK_IS_SELECTION_MODEL (model))
        return FALSE;
    gtk_selection_model_select_item (GTK_SELECTION_MODEL (model), 0, TRUE);
    auto controllers = gtk_widget_observe_controllers (GTK_WIDGET (view));
    for (guint position = 0;
         position < g_list_model_get_n_items (controllers); ++position)
    {
        auto controller = G_OBJECT (g_list_model_get_item (controllers, position));
        if (GTK_IS_EVENT_CONTROLLER_KEY (controller))
        {
            gboolean handled = FALSE;
            g_signal_emit_by_name (controller, "key-pressed", GDK_KEY_F10,
                                   0u, GDK_SHIFT_MASK, &handled);
            g_object_unref (controller);
            g_object_unref (controllers);
            return handled;
        }
        g_object_unref (controller);
    }
    g_object_unref (controllers);
    return FALSE;
}

static void
collect_widgets_with_class (GtkWidget *widget, const char *css_class,
                            std::vector<GtkWidget*> &widgets)
{
    if (gtk_widget_has_css_class (widget, css_class))
        widgets.push_back (widget);
    for (auto child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        collect_widgets_with_class (child, css_class, widgets);
}

static bool
spin_until_frame (GtkWidget *widget)
{
    gboolean frame_seen = FALSE;
    auto tick_id = gtk_widget_add_tick_callback (
        widget, +[](GtkWidget*, GdkFrameClock*, gpointer data) {
            *static_cast<gboolean*> (data) = TRUE;
            return G_SOURCE_REMOVE;
        }, &frame_seen, nullptr);
    const auto deadline = g_get_monotonic_time () + 2 * G_TIME_SPAN_SECOND;
    do
    {
        g_main_context_iteration (nullptr, FALSE);
        if (frame_seen && gtk_widget_get_realized (widget) &&
            gtk_widget_get_width (widget) > 1 &&
            gtk_widget_get_height (widget) > 1)
            return true;
    }
    while (g_get_monotonic_time () < deadline);
    gtk_widget_remove_tick_callback (widget, tick_id);
    return false;
}

static GtkWidget *
first_descendant_of_type (GtkWidget *widget, GType type)
{
    if (G_TYPE_CHECK_INSTANCE_TYPE (widget, type))
        return widget;
    for (auto child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        if (auto result = first_descendant_of_type (child, type))
            return result;
    return nullptr;
}

static void
collect_descendants_of_type (GtkWidget *widget, GType type,
                             std::vector<GtkWidget*> &widgets)
{
    if (G_TYPE_CHECK_INSTANCE_TYPE (widget, type))
        widgets.push_back (widget);
    for (auto child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        collect_descendants_of_type (child, type, widgets);
}

static GtkWidget *
first_visible_action_cell (GtkWidget *view)
{
    std::vector<GtkWidget*> cells;
    collect_widgets_with_class (view, "gnc-import-matcher-action-cell", cells);
    auto found = std::find_if (cells.begin (), cells.end (),
                              [](GtkWidget *cell) {
        auto button = first_descendant_of_type (cell, GTK_TYPE_CHECK_BUTTON);
        return gtk_widget_get_mapped (cell) && button &&
               gtk_widget_get_visible (button);
    });
    return found == cells.end () ? nullptr : *found;
}

static bool
widget_or_ancestor_has_state (GtkWidget *widget, GtkStateFlags state)
{
    for (auto current = widget; current;
         current = gtk_widget_get_parent (current))
        if (gtk_widget_get_state_flags (current) & state)
            return true;
    return false;
}

static bool
save_matcher_snapshot (GtkWidget *widget, const char *path)
{
    auto paintable = gtk_widget_paintable_new (widget);
    auto snapshot = gtk_snapshot_new ();
    gdk_paintable_snapshot (GDK_PAINTABLE (paintable), snapshot,
                            gtk_widget_get_width (widget),
                            gtk_widget_get_height (widget));
    auto node = gtk_snapshot_free_to_node (snapshot);
    auto native = gtk_widget_get_native (widget);
    auto renderer = native ? gtk_native_get_renderer (native) : nullptr;
    GdkTexture *texture = nullptr;
    if (node && renderer)
    {
        graphene_rect_t bounds;
        graphene_rect_init (&bounds, 0.0f, 0.0f,
                            static_cast<float> (gtk_widget_get_width (widget)),
                            static_cast<float> (gtk_widget_get_height (widget)));
        texture = gsk_renderer_render_texture (renderer, node, &bounds);
    }
    const auto saved = texture && gdk_texture_save_to_png (texture, path);
    g_clear_object (&texture);
    if (node)
        gsk_render_node_unref (node);
    g_object_unref (paintable);
    return saved;
}

static bool
rendered_pixel (GtkWidget *widget, GtkWidget *sample_widget, guint32 *pixel_out)
{
    g_return_val_if_fail (pixel_out, false);
    auto paintable = gtk_widget_paintable_new (widget);
    auto snapshot = gtk_snapshot_new ();
    gdk_paintable_snapshot (GDK_PAINTABLE (paintable), snapshot,
                            gtk_widget_get_width (widget),
                            gtk_widget_get_height (widget));
    auto node = gtk_snapshot_free_to_node (snapshot);
    auto native = gtk_widget_get_native (widget);
    auto renderer = native ? gtk_native_get_renderer (native) : nullptr;
    graphene_rect_t sample_bounds;
    if (!node || !renderer ||
        !gtk_widget_compute_bounds (sample_widget, widget, &sample_bounds))
    {
        if (node)
            gsk_render_node_unref (node);
        g_object_unref (paintable);
        return false;
    }
    graphene_rect_t bounds;
    graphene_rect_init (&bounds, 0.0f, 0.0f,
                        static_cast<float> (gtk_widget_get_width (widget)),
                        static_cast<float> (gtk_widget_get_height (widget)));
    auto texture = gsk_renderer_render_texture (renderer, node, &bounds);
    if (!texture)
    {
        gsk_render_node_unref (node);
        g_object_unref (paintable);
        return false;
    }
    const auto width = gdk_texture_get_width (texture);
    const auto height = gdk_texture_get_height (texture);
    if (width <= 0 || height <= 0)
    {
        g_object_unref (texture);
        gsk_render_node_unref (node);
        g_object_unref (paintable);
        return false;
    }
    std::vector<guchar> pixels (width * height * 4);
    gdk_texture_download (texture, pixels.data (), width * 4);
    auto x = std::clamp (static_cast<int> (sample_bounds.origin.x + 2),
                         0, width - 1);
    auto y = std::clamp (static_cast<int> (sample_bounds.origin.y +
                                            sample_bounds.size.height / 2),
                         0, height - 1);
    std::memcpy (pixel_out, pixels.data () + (y * width + x) * 4, 4);
    g_object_unref (texture);
    gsk_render_node_unref (node);
    g_object_unref (paintable);
    return true;
}

static gboolean
weak_ref_was_finalized (GWeakRef *weak_ref)
{
    auto object = G_OBJECT (g_weak_ref_get (weak_ref));
    auto finalized = object == nullptr;

    g_clear_object (&object);
    g_weak_ref_clear (weak_ref);
    return finalized;
}

static gboolean
wait_until_weak_ref_finalized (GWeakRef *weak_ref)
{
    const auto deadline = g_get_monotonic_time () + 2 * G_TIME_SPAN_SECOND;

    do
    {
        while (g_main_context_pending (nullptr))
            g_main_context_iteration (nullptr, FALSE);
        auto object = G_OBJECT (g_weak_ref_get (weak_ref));
        if (!object)
        {
            g_weak_ref_clear (weak_ref);
            return TRUE;
        }
        g_object_unref (object);
        g_usleep (1000);
    }
    while (g_get_monotonic_time () < deadline);
    g_weak_ref_clear (weak_ref);
    return FALSE;
}

static std::vector<GtkWidget *>
child_popovers (GtkWidget *host)
{
    std::vector<GtkWidget *> popovers;

    for (auto child = gtk_widget_get_first_child (host); child;
         child = gtk_widget_get_next_sibling (child))
        if (GTK_IS_POPOVER (child))
            popovers.emplace_back (child);
    return popovers;
}

struct PopoverLifecycleSnapshot
{
    gboolean floating;
    guint ref_count;
    gboolean parent;
    gboolean mapped;
    gboolean realized;
    gboolean root;
};

static PopoverLifecycleSnapshot
snapshot_popover_lifecycle (GtkWidget *popover)
{
    return { g_object_is_floating (popover), G_OBJECT (popover)->ref_count,
             gtk_widget_get_parent (popover) != nullptr,
             gtk_widget_get_mapped (popover), gtk_widget_get_realized (popover),
             gtk_widget_get_root (popover) != nullptr };
}

static void
log_popover_lifecycle_snapshot (const char *phase,
                                const PopoverLifecycleSnapshot& snapshot)
{
    g_printerr ("%s: floating=%d ref-count=%u parent=%d mapped=%d realized=%d root=%d\n",
                phase, snapshot.floating, snapshot.ref_count, snapshot.parent,
                snapshot.mapped, snapshot.realized, snapshot.root);
}

static gboolean
wait_until_child_popover_count (GtkWidget *host, size_t expected_count)
{
    const auto deadline = g_get_monotonic_time () + 2 * G_TIME_SPAN_SECOND;

    do
    {
        while (g_main_context_pending (nullptr))
            g_main_context_iteration (nullptr, FALSE);
        if (child_popovers (host).size () == expected_count)
            return TRUE;
        g_usleep (1000);
    }
    while (g_get_monotonic_time () < deadline);
    return FALSE;
}

struct TestTransaction
{
    Transaction *transaction;
    Split *split;
};

static TestTransaction
create_test_transaction (QofBook *book, Account *account,
                         gnc_commodity *currency, gint64 amount,
                         char reconcile, gboolean leave_open)
{
    auto transaction = xaccMallocTransaction (book);
    auto split = xaccMallocSplit (book);
    auto value = gnc_numeric_create (amount, 1);

    xaccTransBeginEdit (transaction);
    xaccTransSetCurrency (transaction, currency);
    xaccTransSetDatePostedSecsNormalized (transaction, 1000);
    xaccTransSetDescription (transaction, "selection regression");
    xaccSplitSetParent (split, transaction);
    xaccSplitSetAccount (split, account);
    xaccSplitSetAmount (split, value);
    xaccSplitSetValue (split, value);
    xaccSplitSetReconcile (split, reconcile);
    if (!leave_open)
        xaccTransCommitEdit (transaction);
    return { transaction, split };
}

static Transaction *
create_import_transaction (QofBook *book, Account *source, Account *destination,
                           gnc_commodity *currency, gint64 amount,
                           const char *description, const char *memo,
                           bool balanced)
{
    auto transaction = xaccMallocTransaction (book);
    auto source_split = xaccMallocSplit (book);
    xaccTransBeginEdit (transaction);
    xaccTransSetCurrency (transaction, currency);
    xaccTransSetDatePostedSecsNormalized (transaction, 1000 + amount);
    xaccTransSetDescription (transaction, description);
    xaccSplitSetParent (source_split, transaction);
    xaccSplitSetAccount (source_split, source);
    xaccSplitSetMemo (source_split, memo);
    xaccSplitSetAmount (source_split, gnc_numeric_create (amount, 1));
    xaccSplitSetValue (source_split, gnc_numeric_create (amount, 1));
    if (balanced)
    {
        auto destination_split = xaccMallocSplit (book);
        xaccSplitSetParent (destination_split, transaction);
        xaccSplitSetAccount (destination_split, destination);
        xaccSplitSetAmount (destination_split, gnc_numeric_create (-amount, 1));
        xaccSplitSetValue (destination_split, gnc_numeric_create (-amount, 1));
    }
    return transaction;
}

static GNCImportMatchInfo *
find_match_for_split (GNCImportTransInfo *info, Split *split)
{
    for (auto node = gnc_import_TransInfo_get_match_list (info); node;
         node = g_list_next (node))
    {
        auto match = static_cast<GNCImportMatchInfo*> (node->data);

        if (gnc_import_MatchInfo_get_split (match) == split)
            return match;
    }
    return nullptr;
}

struct MatchPickerResult
{
    guint calls {0};
};

static void
match_picker_done (GNCImportTransInfo *info, gpointer user_data)
{
    auto result = static_cast<MatchPickerResult*> (user_data);

    result->calls++;
    (void)info;
}

struct OfxLifecycleMetrics
{
    guint metadata_cleanup_calls {0};
    guint payload_destroy_calls {0};
    guint reconcile_calls {0};
    GncImportOperationTeardownResult result {
        GNC_IMPORT_OPERATION_TEARDOWN_STALE};
};

struct OfxLifecyclePayload
{
    OfxLifecycleMetrics *metrics;
    GNCImportMainMatcher *matcher {nullptr};
    GList *transactions {nullptr};
};

static void
ofx_lifecycle_payload_destroyed (gpointer user_data)
{
    auto payload = static_cast<OfxLifecyclePayload *> (user_data);
    payload->metrics->payload_destroy_calls++;
    delete payload;
}

static void
ofx_lifecycle_metadata_cleanup (GncOfxImportLifecycle *lifecycle,
                                GncImportOperationTeardownResult result,
                                gpointer user_data)
{
    auto payload = static_cast<OfxLifecyclePayload *> (user_data);
    payload->metrics->metadata_cleanup_calls++;
    payload->metrics->result = result;
    EXPECT_EQ (lifecycle == nullptr, false);
}

static GncOfxImportLifecycle *
create_ofx_lifecycle (QofBook *book, GApplication *application,
                      OfxLifecycleMetrics *metrics,
                      OfxLifecyclePayload **payload_out)
{
    auto context = gnc_session_operation_context_new (
        book, QOF_SESSION_OPERATION_IMPORT);
    if (!context)
        return nullptr;
    auto payload = new OfxLifecyclePayload {metrics};
    auto lifecycle = gnc_ofx_import_lifecycle_new (
        context, application, &payload->matcher, &payload->transactions,
        ofx_lifecycle_metadata_cleanup, payload,
        ofx_lifecycle_payload_destroyed);
    gnc_session_operation_context_unref (context);
    if (payload_out)
        *payload_out = payload;
    return lifecycle;
}

static Transaction *
add_open_transaction (QofBook *book, OfxLifecyclePayload *payload)
{
    auto transaction = xaccMallocTransaction (book);
    xaccTransBeginEdit (transaction);
    payload->transactions = g_list_append (payload->transactions,
                                           transaction);
    return transaction;
}

static bool
wait_for_ofx_cleanup (OfxLifecycleMetrics *metrics, GApplication *application)
{
    gboolean deadline_reached = FALSE;
    auto wakeup = g_timeout_source_new (1000);
    g_source_set_callback (wakeup, +[](gpointer data) {
        *static_cast<gboolean*> (data) = TRUE;
        return G_SOURCE_REMOVE;
    }, &deadline_reached, nullptr);
    if (!g_source_attach (wakeup, nullptr))
    {
        g_source_destroy (wakeup);
        g_source_unref (wakeup);
        g_signal_emit_by_name (application, "shutdown");
        return false;
    }
    const auto deadline = g_get_monotonic_time () + G_TIME_SPAN_SECOND;
    while (metrics->metadata_cleanup_calls == 0 && !deadline_reached &&
           g_get_monotonic_time () < deadline)
        g_main_context_iteration (nullptr, TRUE);
    const auto completed = metrics->metadata_cleanup_calls != 0;
    g_source_destroy (wakeup);
    g_source_unref (wakeup);
    if (!completed)
        g_signal_emit_by_name (application, "shutdown");
    return completed;
}

static void
run_matcher_ofx_cancel_order (QofBook *book, gboolean matcher_first)
{
    auto application = g_application_new (nullptr, G_APPLICATION_NON_UNIQUE);
    OfxLifecycleMetrics metrics;
    OfxLifecyclePayload *payload = nullptr;
    auto lifecycle = create_ofx_lifecycle (book, application, &metrics,
                                           &payload);
    ASSERT_NE (lifecycle, nullptr);

    payload->matcher = gnc_gen_trans_list_new (nullptr, nullptr, FALSE, 42, FALSE);
    ASSERT_NE (payload->matcher, nullptr);
    ASSERT_TRUE (gnc_gen_trans_list_bind_operation_teardown (
        payload->matcher,
        gnc_ofx_import_lifecycle_get_teardown (lifecycle)));
    add_open_transaction (book, payload);

    auto save_lease = qof_session_operation_lease_acquire_for (
        gnc_get_current_session (), QOF_SESSION_OPERATION_SAVE);
    ASSERT_NE (save_lease, nullptr);
    if (matcher_first)
    {
        gnc_gen_trans_list_delete (payload->matcher);
        EXPECT_FALSE (gnc_ofx_import_lifecycle_request (lifecycle));
    }
    else
    {
        EXPECT_FALSE (gnc_ofx_import_lifecycle_request (lifecycle));
        gnc_gen_trans_list_delete (payload->matcher);
    }
    EXPECT_FALSE (gnc_ofx_import_lifecycle_request (lifecycle));
    EXPECT_EQ (metrics.metadata_cleanup_calls, 0u);

    qof_session_operation_lease_release (save_lease);
    if (!wait_for_ofx_cleanup (&metrics, application))
    {
        g_object_unref (application);
        FAIL () << "OFX cleanup did not complete before its deadline";
    }

    EXPECT_EQ (metrics.metadata_cleanup_calls, 1u);
    EXPECT_EQ (metrics.payload_destroy_calls, 1u);
    EXPECT_EQ (metrics.result,
               GNC_IMPORT_OPERATION_TEARDOWN_MUTATION_ALLOWED);
    for (guint turn = 0; turn < 3; ++turn)
        g_main_context_iteration (nullptr, FALSE);
    EXPECT_EQ (metrics.metadata_cleanup_calls, 1u);
    EXPECT_EQ (metrics.payload_destroy_calls, 1u);
    g_object_unref (application);
}

static void
reconcile_continuation_called (GObject *source, gpointer user_data)
{
    auto metrics = static_cast<OfxLifecycleMetrics *> (user_data);
    metrics->reconcile_calls++;
    (void)source;
}

TEST_F(ImportMatcherTest, matcher_cells_keep_layout_status_and_action_invariants)
{
    const std::string long_text (600, 'x');
    using MatcherPtr = std::unique_ptr<GNCImportMainMatcher,
                                       decltype (&gnc_gen_trans_list_delete)>;
    MatcherPtr matcher {gnc_gen_trans_list_new (nullptr, "Synthetic import",
                                                FALSE, 42, FALSE),
                        gnc_gen_trans_list_delete};
    ASSERT_NE (matcher, nullptr);
    for (gint64 index = 1; index <= 36; ++index)
    {
        auto description = "Synthetic groceries transaction " +
                           std::to_string (index);
        auto memo = "Synthetic card memo " + std::to_string (index);
        if (index >= 35)
        {
            description += " " + long_text;
            memo += " " + long_text;
        }
        gnc_gen_trans_list_add_trans (
            matcher.get (), create_import_transaction (
                m_book, m_bank, m_expenses, m_currency, index,
                description.c_str (), memo.c_str (),
                index <= 2 || index % 2));
    }

    gnc_gen_trans_list_show_all (matcher.get ());
    auto window = gnc_gen_trans_list_widget (matcher.get ());
    ASSERT_TRUE (GTK_IS_WINDOW (window));
    gtk_window_set_default_size (GTK_WINDOW (window), 960, 560);
    gtk_window_present (GTK_WINDOW (window));
    ASSERT_TRUE (spin_until_frame (window));

    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        window, "scrolledwindow25"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    auto view = gtk_scrolled_window_get_child (scroller);
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));

    std::vector<GtkWidget*> cells;
    collect_widgets_with_class (view, "gnc-import-matcher-cell", cells);
    ASSERT_GT (cells.size (), 8u);
    int first_height = 0;
    constexpr const char *status_classes[] = {
        "gnc-class-intervention-required",
        "gnc-class-intervention-probably-required",
        "gnc-class-intervention-not-required",
        "gnc-class-intervention-required-dark",
        "gnc-class-intervention-probably-required-dark",
        "gnc-class-intervention-not-required-dark"};
    cells.erase (std::remove_if (cells.begin (), cells.end (),
                                [](GtkWidget *cell) {
                                    return !gtk_widget_get_mapped (cell) ||
                                           gtk_widget_get_width (cell) <= 0 ||
                                           gtk_widget_get_height (cell) <= 0;
                                }), cells.end ());
    ASSERT_GT (cells.size (), 8u);
    for (auto cell : cells)
    {
        auto parent = gtk_widget_get_parent (cell);
        ASSERT_NE (parent, nullptr);
        GtkBorder padding;
        G_GNUC_BEGIN_IGNORE_DEPRECATIONS
        gtk_style_context_get_padding (gtk_widget_get_style_context (parent),
                                       &padding);
        G_GNUC_END_IGNORE_DEPRECATIONS
        EXPECT_EQ (padding.left, 0);
        EXPECT_EQ (padding.right, 0);
        EXPECT_EQ (padding.top, 0);
        EXPECT_EQ (padding.bottom, 0);
        EXPECT_EQ (gtk_widget_get_overflow (parent), GTK_OVERFLOW_HIDDEN);
        graphene_rect_t cell_bounds;
        ASSERT_TRUE (gtk_widget_compute_bounds (cell, parent, &cell_bounds));
        /* The factory cell must cover the parent's clipped client area.
         * A larger minimum/CSS border box is valid as long as no inner edge
         * is left uncovered. */
        EXPECT_LE (std::ceil (cell_bounds.origin.x), 0.0);
        EXPECT_LE (std::ceil (cell_bounds.origin.y), 0.0);
        EXPECT_GE (std::floor (cell_bounds.origin.x +
                               cell_bounds.size.width),
                   gtk_widget_get_width (parent));
        EXPECT_GE (std::floor (cell_bounds.origin.y +
                               cell_bounds.size.height),
                   gtk_widget_get_height (parent));
        if (!first_height)
            first_height = static_cast<int> (cell_bounds.size.height);
        EXPECT_LE (std::abs (cell_bounds.size.height - first_height), 1.0);
        auto status_count = std::count_if (
            std::begin (status_classes), std::end (status_classes),
            [cell](const char *name) { return gtk_widget_has_css_class (cell, name); });
        EXPECT_EQ (status_count, 1);
        if (gtk_widget_has_css_class (cell, "gnc-import-matcher-action-cell"))
        {
            auto button = first_descendant_of_type (cell, GTK_TYPE_CHECK_BUTTON);
            ASSERT_NE (button, nullptr);
            graphene_rect_t bounds;
            ASSERT_TRUE (gtk_widget_compute_bounds (button, cell, &bounds));
            EXPECT_LE (std::abs (bounds.origin.x -
                                 (gtk_widget_get_width (cell) -
                                  bounds.size.width) / 2.0), 1.0);
        }
    }

    int minimum = 0;
    int natural = 0;
    gtk_widget_measure (view, GTK_ORIENTATION_HORIZONTAL, -1,
                        &minimum, &natural, nullptr, nullptr);
    EXPECT_LT (natural, 2400);

    auto selection = GTK_MULTI_SELECTION (
        gtk_column_view_get_model (GTK_COLUMN_VIEW (view)));
    ASSERT_TRUE (GTK_IS_MULTI_SELECTION (selection));
    auto action_cell = first_visible_action_cell (view);
    ASSERT_NE (action_cell, nullptr);
    auto action = GTK_CHECK_BUTTON (first_descendant_of_type (
        action_cell, GTK_TYPE_CHECK_BUTTON));
    auto action_before = gtk_check_button_get_active (action);
    auto status_class = [&status_classes] (GtkWidget *cell) -> const char* {
        for (auto name : status_classes)
            if (gtk_widget_has_css_class (cell, name))
                return name;
        return nullptr;
    };
    ASSERT_NE (status_class (action_cell), nullptr);
    auto status_before = std::string {status_class (action_cell)};
    guint32 pixel_before_selection = 0;
    ASSERT_TRUE (rendered_pixel (view, action_cell,
                                 &pixel_before_selection));
    gtk_selection_model_select_item (GTK_SELECTION_MODEL (selection), 0, TRUE);
    ASSERT_TRUE (spin_until_frame (view));
    EXPECT_TRUE (gtk_selection_model_is_selected (
        GTK_SELECTION_MODEL (selection), 0));
    EXPECT_EQ (gtk_check_button_get_active (action), action_before);
    EXPECT_TRUE (widget_or_ancestor_has_state (action_cell,
                                                GTK_STATE_FLAG_SELECTED));
    guint32 pixel_after_selection = 0;
    ASSERT_TRUE (rendered_pixel (view, action_cell,
                                 &pixel_after_selection));
    EXPECT_NE (pixel_after_selection, pixel_before_selection);
    gtk_selection_model_select_item (GTK_SELECTION_MODEL (selection), 1,
                                     FALSE);
    ASSERT_TRUE (spin_until_frame (view));
    ASSERT_TRUE (gtk_selection_model_is_selected (
        GTK_SELECTION_MODEL (selection), 0));
    ASSERT_TRUE (gtk_selection_model_is_selected (
        GTK_SELECTION_MODEL (selection), 1));
    g_object_ref (action_cell);
    g_object_ref (action);
    gtk_check_button_set_active (action, !action_before);
    auto rebound_after_disable = spin_until_frame (view);
    g_object_unref (action);
    g_object_unref (action_cell);
    ASSERT_TRUE (rebound_after_disable);
    action_cell = first_visible_action_cell (view);
    ASSERT_NE (action_cell, nullptr);
    EXPECT_TRUE (gtk_selection_model_is_selected (
        GTK_SELECTION_MODEL (selection), 0));
    EXPECT_FALSE (gtk_selection_model_is_selected (
        GTK_SELECTION_MODEL (selection), 1));
    action = GTK_CHECK_BUTTON (first_descendant_of_type (
        action_cell, GTK_TYPE_CHECK_BUTTON));
    ASSERT_NE (status_class (action_cell), nullptr);
    EXPECT_NE (status_before, status_class (action_cell));
    g_object_ref (action_cell);
    g_object_ref (action);
    gtk_check_button_set_active (action, action_before);
    auto rebound_after_restore = spin_until_frame (view);
    g_object_unref (action);
    g_object_unref (action_cell);
    ASSERT_TRUE (rebound_after_restore);
    action_cell = first_visible_action_cell (view);
    ASSERT_NE (action_cell, nullptr);
    EXPECT_TRUE (gtk_selection_model_is_selected (
        GTK_SELECTION_MODEL (selection), 0));
    action = GTK_CHECK_BUTTON (first_descendant_of_type (
        action_cell, GTK_TYPE_CHECK_BUTTON));
    EXPECT_EQ (gtk_check_button_get_active (action), action_before);
    ASSERT_NE (status_class (action_cell), nullptr);
    EXPECT_EQ (status_before, status_class (action_cell));
    EXPECT_TRUE (widget_or_ancestor_has_state (action_cell,
                                                GTK_STATE_FLAG_SELECTED));

    auto adjustment = gtk_scrolled_window_get_vadjustment (scroller);
    auto top = gtk_adjustment_get_value (adjustment);
    auto bottom = std::max (gtk_adjustment_get_lower (adjustment),
                            gtk_adjustment_get_upper (adjustment) -
                            gtk_adjustment_get_page_size (adjustment));
    ASSERT_GT (bottom, top);
    gtk_adjustment_set_value (adjustment, bottom);
    ASSERT_TRUE (spin_until_frame (view));
    EXPECT_GT (gtk_adjustment_get_value (adjustment), top);
    gtk_adjustment_set_value (adjustment, gtk_adjustment_get_lower (adjustment));
    ASSERT_TRUE (spin_until_frame (view));
    EXPECT_LE (gtk_adjustment_get_value (adjustment), top);
    cells.clear ();
    collect_widgets_with_class (view, "gnc-import-matcher-cell", cells);
    cells.erase (std::remove_if (cells.begin (), cells.end (),
                                [](GtkWidget *cell) {
                                    return !gtk_widget_get_mapped (cell) ||
                                           gtk_widget_get_width (cell) <= 0 ||
                                           gtk_widget_get_height (cell) <= 0;
                                }), cells.end ());
    ASSERT_GT (cells.size (), 8u);
    for (auto cell : cells)
        EXPECT_EQ (std::count_if (
            std::begin (status_classes), std::end (status_classes),
            [cell](const char *name) { return gtk_widget_has_css_class (cell, name); }), 1);

    if (auto snapshot_path = g_getenv ("GNC_TEST_IMPORT_MATCHER_SNAPSHOT"))
    {
        ASSERT_TRUE (g_path_is_absolute (snapshot_path));
        ASSERT_TRUE (spin_until_frame (window));
        EXPECT_TRUE (save_matcher_snapshot (window, snapshot_path));
    }
}

TEST_F(ImportMatcherTest, embedded_matcher_ignores_late_account_picker_completion)
{
    auto window = GTK_WINDOW (gtk_window_new ());
    g_object_ref (window);
    auto page = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_window_set_child (window, page);
    auto matcher = gnc_gen_trans_assist_new (GTK_WIDGET (window), page,
                                             "Embedded matcher", FALSE, 42);
    ASSERT_NE (matcher, nullptr);
    gnc_gen_trans_list_add_trans (matcher, create_import_transaction (
        m_book, m_bank, m_expenses, m_currency, 700,
        "unbalanced account-picker lifetime", "", FALSE));
    gnc_gen_trans_list_show_all (matcher);
    gtk_window_present (window);
    ASSERT_TRUE (spin_until_frame (GTK_WIDGET (window)));

    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "scrolledwindow25"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    GWeakRef content_ref;
    g_weak_ref_init (&content_ref, G_OBJECT (scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));

    /* Row activation is the product path for assigning an account to an
     * unbalanced imported transaction. It opens the asynchronous picker. */
    g_signal_emit_by_name (view, "activate", 0u);
    auto picker = find_buildable_window ("account_picker_dialog");
    ASSERT_NE (picker, nullptr);
    auto picker_scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (picker), "account_tree_sw"));
    auto accept = find_buildable_widget (GTK_WIDGET (picker), "okbutton");
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (picker_scroller));
    ASSERT_TRUE (GTK_IS_BUTTON (accept));
    auto picker_view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (picker_scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (picker_view));
    auto selection = GTK_SINGLE_SELECTION (gtk_column_view_get_model (picker_view));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (selection));
    auto bank_position = account_picker_position (selection, m_bank);
    ASSERT_NE (bank_position, GTK_INVALID_LIST_POSITION);
    gtk_single_selection_set_selected (selection, bank_position);
    GWeakRef picker_ref;
    g_weak_ref_init (&picker_ref, G_OBJECT (picker));

    /* The parent assistant remains alive, so a window WeakRef alone would not
     * protect the callback. The real picker completion must become a no-op. */
    gnc_gen_trans_list_delete (matcher);
    EXPECT_TRUE (GTK_IS_WINDOW (window));
    gtk_window_set_default_size (window, 640, 420);
    EXPECT_TRUE (spin_until_frame (GTK_WIDGET (window)));
    g_signal_emit_by_name (accept, "clicked");
    EXPECT_TRUE (wait_until_buildable_window_closed ("account_picker_dialog"));
    g_object_unref (picker);
    EXPECT_TRUE (weak_ref_was_finalized (&picker_ref));
    /* The asynchronous account-selection closure legitimately retains its
     * selected rows while the picker is open. The matcher is already
     * invalidated above; require its detached content to be released after
     * the actual completion path has run. */
    EXPECT_TRUE (wait_until_weak_ref_finalized (&content_ref));
    gtk_window_destroy (window);
    g_object_unref (window);
}

TEST_F(ImportMatcherTest, embedded_matcher_cancels_replaced_and_torn_down_match_picker)
{
    auto existing = create_test_transaction (m_book, m_bank, m_currency,
                                             1, NREC, FALSE);
    auto window = GTK_WINDOW (gtk_window_new ());
    g_object_ref (window);
    auto page = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_window_set_child (window, page);
    auto matcher = gnc_gen_trans_assist_new (GTK_WIDGET (window), page,
                                             "Embedded matcher", FALSE, 42);
    ASSERT_NE (matcher, nullptr);
    gnc_gen_trans_list_add_trans (matcher, create_import_transaction (
        m_book, m_bank, m_expenses, m_currency, 1,
        "matching picker lifetime", "", TRUE));
    gnc_gen_trans_list_show_all (matcher);
    gtk_window_present (window);
    ASSERT_TRUE (spin_until_frame (GTK_WIDGET (window)));

    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "scrolledwindow25"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    GWeakRef content_ref;
    g_weak_ref_init (&content_ref, G_OBJECT (scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));

    /* A matched row activates the product Match-Picker path. Opening it twice
     * must consume the first borrowed-data picker before replacing it. */
    g_signal_emit_by_name (view, "activate", 0u);
    ASSERT_EQ (count_buildable_windows ("match_picker_dialog"), 1u);
    g_signal_emit_by_name (view, "activate", 0u);
    ASSERT_EQ (count_buildable_windows ("match_picker_dialog"), 1u);

    gnc_gen_trans_list_delete (matcher);
    EXPECT_TRUE (wait_until_buildable_window_closed ("match_picker_dialog"));
    EXPECT_TRUE (weak_ref_was_finalized (&content_ref));
    gtk_window_set_default_size (window, 640, 420);
    EXPECT_TRUE (spin_until_frame (GTK_WIDGET (window)));
    gtk_window_destroy (window);
    g_object_unref (window);
    (void)existing;
}

TEST_F(ImportMatcherTest, embedded_matcher_ignores_late_edit_fields_accept)
{
    auto window = GTK_WINDOW (gtk_window_new ());
    g_object_ref (window);
    auto page = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_window_set_child (window, page);
    auto matcher = gnc_gen_trans_assist_new (GTK_WIDGET (window), page,
                                             "Embedded matcher", FALSE, 42);
    ASSERT_NE (matcher, nullptr);
    gnc_gen_trans_list_add_trans (matcher, create_import_transaction (
        m_book, m_bank, m_expenses, m_currency, 702,
        "late edit-fields lifetime", "memo", TRUE));
    gnc_gen_trans_list_show_all (matcher);
    gtk_window_present (window);
    ASSERT_TRUE (spin_until_frame (GTK_WIDGET (window)));

    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "scrolledwindow25"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    ASSERT_TRUE (open_matcher_context_menu (view));
    auto edit = find_button_with_label (GTK_WIDGET (view),
                                        "_Edit description, notes, or memo");
    ASSERT_TRUE (GTK_IS_BUTTON (edit));
    g_signal_emit_by_name (edit, "clicked");
    auto dialog = find_buildable_window ("transaction_edit_dialog");
    ASSERT_NE (dialog, nullptr);
    auto entry = GTK_ENTRY (first_descendant_of_type (GTK_WIDGET (dialog), GTK_TYPE_ENTRY));
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    ASSERT_TRUE (spin_until_frame (GTK_WIDGET (dialog)));
    auto focused = gtk_root_get_focus (GTK_ROOT (dialog));
    ASSERT_NE (focused, nullptr);
    ASSERT_TRUE (focused == GTK_WIDGET (entry) ||
                 gtk_widget_is_ancestor (focused, GTK_WIDGET (entry)));
    gtk_editable_set_text (GTK_EDITABLE (entry), "late");
    std::vector<GtkWidget *> suggestion_popovers;
    collect_descendants_of_type (GTK_WIDGET (dialog), GTK_TYPE_POPOVER,
                                 suggestion_popovers);
    ASSERT_EQ (suggestion_popovers.size (), 3u);
    std::array<GWeakRef, 3> suggestion_refs {};
    std::array<GWeakRef, 3> suggestion_model_refs {};
    for (size_t position = 0; position < suggestion_refs.size (); ++position)
    {
        g_weak_ref_init (&suggestion_refs[position], G_OBJECT (suggestion_popovers[position]));
        auto list = GTK_LIST_VIEW (gtk_popover_get_child (
            GTK_POPOVER (suggestion_popovers[position])));
        ASSERT_TRUE (GTK_IS_LIST_VIEW (list));
        auto selection = GTK_SINGLE_SELECTION (gtk_list_view_get_model (list));
        ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (selection));
        auto model = gtk_single_selection_get_model (selection);
        ASSERT_TRUE (G_IS_LIST_STORE (model));
        g_weak_ref_init (&suggestion_model_refs[position], G_OBJECT (model));
    }
    auto accept = find_buildable_widget (GTK_WIDGET (dialog), "button2");
    ASSERT_TRUE (GTK_IS_BUTTON (accept));
    GWeakRef dialog_ref;
    g_weak_ref_init (&dialog_ref, G_OBJECT (dialog));
    g_object_ref (accept);

    gnc_gen_trans_list_delete (matcher);
    EXPECT_TRUE (GTK_IS_WINDOW (window));
    g_signal_emit_by_name (accept, "clicked");
    EXPECT_TRUE (wait_until_buildable_window_closed ("transaction_edit_dialog"));
    gboolean late_click_seen = FALSE;
    auto late_click_handler = g_signal_connect (
        accept, "clicked", G_CALLBACK (+[](GtkButton*, gpointer data) {
            *static_cast<gboolean*> (data) = TRUE;
        }), &late_click_seen);
    g_signal_emit_by_name (accept, "clicked");
    EXPECT_TRUE (late_click_seen);
    g_signal_handler_disconnect (accept, late_click_handler);
    g_object_unref (accept);
    g_object_unref (dialog);
    EXPECT_TRUE (weak_ref_was_finalized (&dialog_ref));
    for (auto& suggestion_ref : suggestion_refs)
        EXPECT_TRUE (wait_until_weak_ref_finalized (&suggestion_ref));
    for (auto& model_ref : suggestion_model_refs)
        EXPECT_TRUE (wait_until_weak_ref_finalized (&model_ref));
    gtk_window_destroy (window);
    g_object_unref (window);
}

TEST_F(ImportMatcherTest, embedded_matcher_ignores_late_price_dialog_accept)
{
    auto euro = gnc_commodity_table_lookup (
        gnc_commodity_table_get_table (m_book), GNC_COMMODITY_NS_CURRENCY,
        "EUR");
    ASSERT_NE (euro, nullptr);
    xaccAccountBeginEdit (m_expenses);
    xaccAccountSetCommodity (m_expenses, euro);
    xaccAccountCommitEdit (m_expenses);

    auto window = GTK_WINDOW (gtk_window_new ());
    g_object_ref (window);
    auto page = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_window_set_child (window, page);
    auto matcher = gnc_gen_trans_assist_new (GTK_WIDGET (window), page,
                                             "Embedded matcher", FALSE, 42);
    ASSERT_NE (matcher, nullptr);
    gnc_gen_trans_list_add_trans (matcher, create_import_transaction (
        m_book, m_bank, m_expenses, m_currency, 703,
        "late price-dialog lifetime", "", FALSE));
    gnc_gen_trans_list_show_all (matcher);
    gtk_window_present (window);
    ASSERT_TRUE (spin_until_frame (GTK_WIDGET (window)));

    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "scrolledwindow25"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    ASSERT_TRUE (open_matcher_context_menu (view));
    auto assign_account = find_button_with_label (GTK_WIDGET (view),
                                                  "_Assign transfer account");
    ASSERT_TRUE (GTK_IS_BUTTON (assign_account));
    ASSERT_TRUE (gtk_widget_get_sensitive (assign_account));
    g_signal_emit_by_name (assign_account, "clicked");
    auto picker = find_buildable_window ("account_picker_dialog");
    ASSERT_NE (picker, nullptr);
    auto picker_scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (picker), "account_tree_sw"));
    auto picker_accept = find_buildable_widget (GTK_WIDGET (picker), "okbutton");
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (picker_scroller));
    ASSERT_TRUE (GTK_IS_BUTTON (picker_accept));
    auto picker_view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (picker_scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (picker_view));
    auto selection = GTK_SINGLE_SELECTION (gtk_column_view_get_model (picker_view));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (selection));
    auto expenses_position = account_picker_position (selection, m_expenses);
    ASSERT_NE (expenses_position, GTK_INVALID_LIST_POSITION);
    gtk_single_selection_set_selected (selection, expenses_position);
    g_signal_emit_by_name (picker_accept, "clicked");
    EXPECT_TRUE (wait_until_buildable_window_closed ("account_picker_dialog"));
    g_object_unref (picker);

    ASSERT_TRUE (open_matcher_context_menu (view));
    auto price = find_button_with_label (GTK_WIDGET (view),
                                         "Assign e_xchange rate");
    ASSERT_TRUE (GTK_IS_BUTTON (price));
    ASSERT_TRUE (gtk_widget_get_sensitive (price));
    g_signal_emit_by_name (price, "clicked");
    auto dialog = find_buildable_window ("transfer_dialog");
    ASSERT_NE (dialog, nullptr);
    auto accept = find_buildable_widget (GTK_WIDGET (dialog), "ok_button");
    ASSERT_TRUE (GTK_IS_BUTTON (accept));
    auto price_box = find_buildable_widget (GTK_WIDGET (dialog), "price_hbox");
    ASSERT_TRUE (GTK_IS_BOX (price_box));
    auto price_edit = GNC_AMOUNT_EDIT (first_descendant_of_type (
        price_box, GNC_TYPE_AMOUNT_EDIT));
    ASSERT_TRUE (GNC_IS_AMOUNT_EDIT (price_edit));
    gnc_amount_edit_set_amount (price_edit, gnc_numeric_create (1, 1));
    GWeakRef dialog_ref;
    g_weak_ref_init (&dialog_ref, G_OBJECT (dialog));

    gnc_gen_trans_list_delete (matcher);
    EXPECT_TRUE (GTK_IS_WINDOW (window));
    g_signal_emit_by_name (accept, "clicked");
    EXPECT_TRUE (wait_until_buildable_window_closed ("transfer_dialog"));
    /* Keep the toplevel reference until after the valid OK path. Its release
     * disposes the price-entry focus controller, which must no longer retain
     * a callback to the transfer dialog's freed state. */
    g_object_unref (dialog);
    EXPECT_TRUE (weak_ref_was_finalized (&dialog_ref));
    gtk_window_destroy (window);
    g_object_unref (window);
}

TEST_F(ImportMatcherTest, embedded_matcher_releases_context_popovers_on_replacement_and_teardown)
{
    auto window = GTK_WINDOW (gtk_window_new ());
    g_object_ref (window);
    auto page = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_window_set_child (window, page);
    auto matcher = gnc_gen_trans_assist_new (GTK_WIDGET (window), page,
                                             "Embedded matcher", FALSE, 42);
    ASSERT_NE (matcher, nullptr);
    gnc_gen_trans_list_add_trans (matcher, create_import_transaction (
        m_book, m_bank, m_expenses, m_currency, 704,
        "context-popover lifetime", "", FALSE));
    gnc_gen_trans_list_show_all (matcher);
    gtk_window_present (window);
    ASSERT_TRUE (spin_until_frame (GTK_WIDGET (window)));

    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "scrolledwindow25"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    ASSERT_TRUE (open_matcher_context_menu (view));
    auto first_popovers = child_popovers (GTK_WIDGET (view));
    ASSERT_EQ (first_popovers.size (), 1u);
    auto closed_popover = first_popovers.front ();
    GWeakRef closed_popover_ref;
    g_weak_ref_init (&closed_popover_ref, G_OBJECT (closed_popover));
    auto delayed_finalization_ref = G_OBJECT (g_object_ref (closed_popover));
    auto initial_snapshot = snapshot_popover_lifecycle (closed_popover);
    gtk_popover_popdown (GTK_POPOVER (closed_popover));
    EXPECT_TRUE (wait_until_child_popover_count (GTK_WIDGET (view), 0u));
    EXPECT_EQ (gtk_widget_get_parent (closed_popover), nullptr);
    EXPECT_FALSE (gtk_widget_get_visible (closed_popover));
    auto unparented_snapshot = snapshot_popover_lifecycle (closed_popover);

    ASSERT_TRUE (open_matcher_context_menu (view));
    auto reopened_snapshot = snapshot_popover_lifecycle (closed_popover);
    GWeakRef diagnostic_popover_ref;
    g_weak_ref_init (&diagnostic_popover_ref, G_OBJECT (closed_popover));
    g_object_unref (delayed_finalization_ref);
    auto closed_popover_finalized = wait_until_weak_ref_finalized (&closed_popover_ref);
    if (!closed_popover_finalized)
    {
        log_popover_lifecycle_snapshot ("context popover initial", initial_snapshot);
        log_popover_lifecycle_snapshot ("context popover unparented", unparented_snapshot);
        log_popover_lifecycle_snapshot ("context popover after reopen", reopened_snapshot);
        auto retained = GTK_WIDGET (g_weak_ref_get (&diagnostic_popover_ref));
        if (retained)
        {
            log_popover_lifecycle_snapshot ("context popover after releasing test ref",
                                            snapshot_popover_lifecycle (retained));
            g_object_unref (retained);
        }
        else
            g_printerr ("context popover finalized after timeout observation\n");
    }
    g_weak_ref_clear (&diagnostic_popover_ref);
    EXPECT_TRUE (closed_popover_finalized);
    auto replaced_popovers = child_popovers (GTK_WIDGET (view));
    ASSERT_EQ (replaced_popovers.size (), 1u);
    GWeakRef replaced_popover_ref;
    g_weak_ref_init (&replaced_popover_ref, G_OBJECT (replaced_popovers.front ()));

    ASSERT_TRUE (open_matcher_context_menu (view));
    EXPECT_TRUE (wait_until_weak_ref_finalized (&replaced_popover_ref));
    auto active_popovers = child_popovers (GTK_WIDGET (view));
    ASSERT_EQ (active_popovers.size (), 1u);
    GWeakRef active_popover_ref;
    g_weak_ref_init (&active_popover_ref, G_OBJECT (active_popovers.front ()));

    gnc_gen_trans_list_delete (matcher);
    EXPECT_TRUE (wait_until_weak_ref_finalized (&active_popover_ref));
    gtk_window_destroy (window);
    g_object_unref (window);
}

TEST_F(ImportMatcherTest, embedded_matcher_ignores_late_context_menu_buttons)
{
    constexpr std::array<const char *, 4> labels {
        "_Assign transfer account",
        "Assign e_xchange rate",
        "_Edit description, notes, or memo",
        "_Reset all edits"
    };
    constexpr std::array<const char *, 3> dialog_ids {
        "account_picker_dialog",
        "transfer_dialog",
        "transaction_edit_dialog"
    };
    std::array<GtkWidget *, labels.size ()> buttons {};
    std::array<gulong, labels.size ()> observer_ids {};
    std::array<guint, labels.size ()> observer_calls {};
    std::array<guint, dialog_ids.size ()> dialog_counts {};
    auto window = GTK_WINDOW (gtk_window_new ());
    g_object_ref (window);
    auto page = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_window_set_child (window, page);
    auto matcher = gnc_gen_trans_assist_new (GTK_WIDGET (window), page,
                                             "Embedded matcher", FALSE, 42);
    ASSERT_NE (matcher, nullptr);
    auto transaction = create_import_transaction (
        m_book, m_bank, m_expenses, m_currency, 705,
        "original context-menu description", "memo", FALSE);
    GncGUID transaction_guid = *qof_instance_get_guid
        (QOF_INSTANCE (transaction));
    gnc_gen_trans_list_add_trans (matcher, transaction);
    gnc_gen_trans_list_show_all (matcher);
    gtk_window_present (window);
    ASSERT_TRUE (spin_until_frame (GTK_WIDGET (window)));

    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "scrolledwindow25"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    ASSERT_TRUE (open_matcher_context_menu (view));
    auto popovers = child_popovers (GTK_WIDGET (view));
    ASSERT_EQ (popovers.size (), 1u);
    auto popover = GTK_POPOVER (g_object_ref (popovers.front ()));
    for (size_t index = 0; index < labels.size (); ++index)
    {
        buttons[index] = find_button_with_label (GTK_WIDGET (view), labels[index]);
        ASSERT_TRUE (GTK_IS_BUTTON (buttons[index]));
        g_object_ref (buttons[index]);
        observer_ids[index] = g_signal_connect (
            buttons[index], "clicked",
            G_CALLBACK (+[](GtkButton*, gpointer data) {
                (*static_cast<guint *> (data))++;
            }), &observer_calls[index]);
    }
    for (size_t index = 0; index < dialog_ids.size (); ++index)
        dialog_counts[index] = count_buildable_windows (dialog_ids[index]);

    gtk_popover_popdown (popover);
    EXPECT_TRUE (wait_until_child_popover_count (GTK_WIDGET (view), 0u));
    EXPECT_EQ (gtk_widget_get_parent (GTK_WIDGET (popover)), nullptr);
    gnc_gen_trans_list_delete (matcher);
    EXPECT_EQ (xaccTransLookup (&transaction_guid, m_book), nullptr);
    for (size_t index = 0; index < buttons.size (); ++index)
    {
        g_signal_emit_by_name (buttons[index], "clicked");
        EXPECT_EQ (observer_calls[index], 1u);
    }
    for (size_t index = 0; index < dialog_ids.size (); ++index)
        EXPECT_EQ (count_buildable_windows (dialog_ids[index]),
                   dialog_counts[index]);
    EXPECT_EQ (xaccTransLookup (&transaction_guid, m_book), nullptr);

    for (size_t index = 0; index < buttons.size (); ++index)
    {
        g_signal_handler_disconnect (buttons[index], observer_ids[index]);
        g_object_unref (buttons[index]);
    }
    g_object_unref (popover);
    gtk_window_destroy (window);
    g_object_unref (window);
}

TEST_F(ImportMatcherTest, test_simple_match)
{
    auto found = gnc_import_select_account(nullptr, "Bank", FALSE, nullptr,
                                           nullptr, ACCT_TYPE_NONE, nullptr,
                                           nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("Bank", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_async_match)
{
    AccountSelectionResult result;

    gnc_import_select_account_async(nullptr, "Bank", FALSE, nullptr,
                                    nullptr, ACCT_TYPE_NONE, nullptr,
                                    account_selected, &result);
    ASSERT_TRUE(result.accepted);
    ASSERT_NE(nullptr, result.account);
    EXPECT_STREQ("Bank", xaccAccountGetName(result.account));
}

TEST_F(ImportMatcherTest, test_async_unmatched_without_prompt)
{
    AccountSelectionResult result;

    gnc_import_select_account_async(nullptr, "Missing", FALSE, nullptr,
                                    nullptr, ACCT_TYPE_NONE, nullptr,
                                    account_selected, &result);
    EXPECT_FALSE(result.accepted);
    EXPECT_EQ(nullptr, result.account);
}

TEST_F(ImportMatcherTest, match_picker_can_be_cancelled_before_borrowed_data_is_released)
{
    auto imported = create_test_transaction (m_book, m_bank, m_currency,
                                             701, NREC, TRUE);
    auto trans_info = gnc_import_TransInfo_new (imported.transaction, m_bank);
    auto pending_matches = gnc_import_PendingMatches_new ();
    MatchPickerResult result;

    auto picker = gnc_import_match_picker_run (nullptr, trans_info,
                                                pending_matches,
                                                match_picker_done, &result);
    ASSERT_NE (picker, nullptr);
    auto window = find_buildable_window ("match_picker_dialog");
    ASSERT_NE (window, nullptr);

    gnc_import_match_picker_cancel (picker);
    EXPECT_EQ (result.calls, 1u);
    g_object_unref (window);
    gnc_import_PendingMatches_delete (pending_matches);
    gnc_import_TransInfo_delete (trans_info);
}

TEST_F(ImportMatcherTest, account_picker_without_default_stays_unselected)
{
    AccountSelectionResult result;
    constexpr auto unmatched_id = "selection-regression-unmatched";

    ASSERT_EQ (xaccAccountGetOnlineID (m_assets), nullptr);
    gnc_import_select_account_async (nullptr, unmatched_id, TRUE,
                                     "Unmatched account", m_currency,
                                     ACCT_TYPE_NONE, nullptr,
                                     account_selected, &result);

    auto window = find_buildable_window ("account_picker_dialog");
    ASSERT_NE (window, nullptr);
    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "account_tree_sw"));
    auto ok_button = find_buildable_widget (GTK_WIDGET (window), "okbutton");
    auto cancel_button = find_buildable_widget (GTK_WIDGET (window), "cancelbutton");
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    ASSERT_TRUE (GTK_IS_BUTTON (ok_button));
    ASSERT_TRUE (GTK_IS_BUTTON (cancel_button));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    auto selection = GTK_SINGLE_SELECTION (gtk_column_view_get_model (view));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (selection));
    g_object_ref (selection);
    GWeakRef selection_ref;
    GWeakRef window_ref;
    g_weak_ref_init (&selection_ref, G_OBJECT (selection));
    g_weak_ref_init (&window_ref, G_OBJECT (window));

    EXPECT_EQ (gtk_single_selection_get_selected (selection),
               GTK_INVALID_LIST_POSITION);
    EXPECT_FALSE (gtk_widget_get_sensitive (ok_button));

    g_signal_emit_by_name (cancel_button, "clicked");
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
    EXPECT_EQ (result.account, nullptr);
    EXPECT_EQ (xaccAccountGetOnlineID (m_assets), nullptr);
    EXPECT_STREQ (xaccAccountGetOnlineID (m_bank), "Bank");
    g_object_unref (window);
    EXPECT_TRUE (weak_ref_was_finalized (&window_ref));
    gtk_single_selection_set_selected (selection, 0);
    EXPECT_EQ (result.calls, 1u);
    g_object_unref (selection);
    EXPECT_TRUE (weak_ref_was_finalized (&selection_ref));
}

TEST_F(ImportMatcherTest, account_picker_preserves_valid_default)
{
    AccountSelectionResult result;

    gnc_import_select_account_async (nullptr, nullptr, TRUE,
                                     "Existing default account", m_currency,
                                     ACCT_TYPE_NONE, m_bank,
                                     account_selected, &result);

    auto window = find_buildable_window ("account_picker_dialog");
    ASSERT_NE (window, nullptr);
    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "account_tree_sw"));
    auto ok_button = find_buildable_widget (GTK_WIDGET (window), "okbutton");
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    ASSERT_TRUE (GTK_IS_BUTTON (ok_button));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    auto selection = GTK_SINGLE_SELECTION (gtk_column_view_get_model (view));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (selection));
    g_object_ref (selection);
    GWeakRef selection_ref;
    GWeakRef window_ref;
    g_weak_ref_init (&selection_ref, G_OBJECT (selection));
    g_weak_ref_init (&window_ref, G_OBJECT (window));

    EXPECT_NE (gtk_single_selection_get_selected (selection),
               GTK_INVALID_LIST_POSITION);
    EXPECT_TRUE (gtk_widget_get_sensitive (ok_button));
    g_signal_emit_by_name (ok_button, "clicked");

    EXPECT_EQ (result.calls, 1u);
    EXPECT_TRUE (result.accepted);
    EXPECT_EQ (result.account, m_bank);
    EXPECT_STREQ (xaccAccountGetOnlineID (m_bank), "Bank");
    g_object_unref (window);
    EXPECT_TRUE (weak_ref_was_finalized (&window_ref));
    gtk_single_selection_set_selected (selection, GTK_INVALID_LIST_POSITION);
    EXPECT_EQ (result.calls, 1u);
    g_object_unref (selection);
    EXPECT_TRUE (weak_ref_was_finalized (&selection_ref));
}

TEST_F(ImportMatcherTest, account_picker_no_mutation_preserves_online_id)
{
    AccountSelectionResult result;
    constexpr auto unmatched_id = "selection-regression-no-mutation";

    ASSERT_STREQ (xaccAccountGetOnlineID (m_bank), "Bank");
    gnc_import_select_account_async_no_mutation (
        nullptr, unmatched_id, TRUE, "Existing default account", m_currency,
        ACCT_TYPE_NONE, m_bank, account_selected, &result);

    auto window = find_buildable_window ("account_picker_dialog");
    ASSERT_NE (window, nullptr);
    auto ok_button = find_buildable_widget (GTK_WIDGET (window), "okbutton");
    ASSERT_TRUE (GTK_IS_BUTTON (ok_button));
    EXPECT_TRUE (gtk_widget_get_sensitive (ok_button));
    g_signal_emit_by_name (ok_button, "clicked");

    EXPECT_EQ (result.calls, 1u);
    EXPECT_TRUE (result.accepted);
    EXPECT_EQ (result.account, m_bank);
    EXPECT_STREQ (xaccAccountGetOnlineID (m_bank), "Bank");
    g_object_unref (window);
    EXPECT_EQ (result.calls, 1u);
}

TEST_F(ImportMatcherTest, account_picker_external_destroy_finishes_once)
{
    AccountSelectionResult result;

    gnc_import_select_account_async (
        nullptr, "selection-regression-external-destroy", TRUE,
        "Externally destroyed picker", m_currency, ACCT_TYPE_NONE, nullptr,
        account_selected, &result);

    auto window = find_buildable_window ("account_picker_dialog");
    ASSERT_NE (window, nullptr);
    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "account_tree_sw"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    auto selection = GTK_SINGLE_SELECTION (gtk_column_view_get_model (view));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (selection));
    g_object_ref (selection);
    GWeakRef selection_ref;
    GWeakRef window_ref;
    g_weak_ref_init (&selection_ref, G_OBJECT (selection));
    g_weak_ref_init (&window_ref, G_OBJECT (window));

    gtk_window_destroy (window);
    EXPECT_EQ (result.calls, 0u);
    g_object_unref (window);

    EXPECT_TRUE (weak_ref_was_finalized (&window_ref));
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
    gtk_single_selection_set_selected (selection, 0);
    EXPECT_EQ (result.calls, 1u);
    g_object_unref (selection);
    EXPECT_TRUE (weak_ref_was_finalized (&selection_ref));
}

TEST_F(ImportMatcherTest, match_picker_does_not_select_first_visible_match)
{
    constexpr auto prefs_group = "dialogs.import.generic.match-picker";
    constexpr auto display_reconciled = "display-reconciled";
    auto previous_display_reconciled = gnc_prefs_get_bool (
        prefs_group, display_reconciled);
    auto imported = create_test_transaction (m_book, m_bank, m_currency,
                                             100, NREC, TRUE);
    auto visible = create_test_transaction (m_book, m_bank, m_currency,
                                            100, NREC, FALSE);
    auto hidden = create_test_transaction (m_book, m_bank, m_currency,
                                           100, YREC, FALSE);
    auto trans_info = gnc_import_TransInfo_new (imported.transaction, m_bank);

    /* split_find_match prepends: add the hidden selection last so that the
     * first displayed row is the other candidate after filtering. */
    split_find_match (trans_info, visible.split, 0, 4, 14, 0.0);
    split_find_match (trans_info, hidden.split, 0, 4, 14, 0.0);
    auto visible_match = find_match_for_split (trans_info, visible.split);
    auto hidden_match = find_match_for_split (trans_info, hidden.split);
    ASSERT_NE (visible_match, nullptr);
    ASSERT_NE (hidden_match, nullptr);
    gnc_import_TransInfo_set_selected_match_info (trans_info, hidden_match, TRUE);
    auto pending_matches = gnc_import_PendingMatches_new ();
    gnc_import_PendingMatches_add_match (pending_matches, hidden_match, TRUE);
    gnc_prefs_set_bool (prefs_group, display_reconciled, FALSE);
    MatchPickerResult result;

    gnc_import_match_picker_run (nullptr, trans_info, pending_matches,
                                 match_picker_done, &result);

    auto window = find_buildable_window ("match_picker_dialog");
    ASSERT_NE (window, nullptr);
    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "matched_view"));
    auto downloaded_scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "download_view"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (downloaded_scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    auto downloaded_view = GTK_COLUMN_VIEW (
        gtk_scrolled_window_get_child (downloaded_scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (downloaded_view));
    auto selection = GTK_SINGLE_SELECTION (gtk_column_view_get_model (view));
    auto downloaded_selection = GTK_SINGLE_SELECTION (
        gtk_column_view_get_model (downloaded_view));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (selection));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (downloaded_selection));
    auto ok_button = gtk_window_get_default_widget (window);
    ASSERT_TRUE (GTK_IS_BUTTON (ok_button));
    g_object_ref (selection);
    g_object_ref (downloaded_selection);
    GWeakRef selection_ref;
    GWeakRef downloaded_selection_ref;
    GWeakRef window_ref;
    g_weak_ref_init (&selection_ref, G_OBJECT (selection));
    g_weak_ref_init (&downloaded_selection_ref,
                     G_OBJECT (downloaded_selection));
    g_weak_ref_init (&window_ref, G_OBJECT (window));
    EXPECT_EQ (g_list_model_get_n_items (G_LIST_MODEL (selection)), 1u);
    EXPECT_EQ (gtk_single_selection_get_selected (selection),
               GTK_INVALID_LIST_POSITION);
    EXPECT_EQ (gnc_import_TransInfo_get_selected_match (trans_info),
               hidden_match);
    EXPECT_EQ (gnc_import_PendingMatches_get_match_type (
                   pending_matches, hidden_match), GNCImportPending_MANUAL);
    EXPECT_EQ (gnc_import_PendingMatches_get_match_type (
                   pending_matches, visible_match), GNCImportPending_NONE);

    g_signal_emit_by_name (ok_button, "clicked");

    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (gnc_import_TransInfo_get_selected_match (trans_info), nullptr);
    EXPECT_EQ (gnc_import_PendingMatches_get_match_type (
                   pending_matches, hidden_match), GNCImportPending_NONE);
    EXPECT_EQ (gnc_import_PendingMatches_get_match_type (
                   pending_matches, visible_match), GNCImportPending_NONE);

    gnc_prefs_set_bool (prefs_group, display_reconciled,
                        previous_display_reconciled);
    g_object_unref (window);
    EXPECT_TRUE (weak_ref_was_finalized (&window_ref));
    gtk_single_selection_set_selected (selection, 0);
    gtk_single_selection_set_selected (downloaded_selection,
                                       GTK_INVALID_LIST_POSITION);
    EXPECT_EQ (result.calls, 1u);
    g_object_unref (downloaded_selection);
    g_object_unref (selection);
    EXPECT_TRUE (weak_ref_was_finalized (&downloaded_selection_ref));
    EXPECT_TRUE (weak_ref_was_finalized (&selection_ref));
    gnc_import_PendingMatches_delete (pending_matches);
    gnc_import_TransInfo_delete (trans_info);
}

TEST_F(ImportMatcherTest, match_picker_score_picture_keeps_native_score_geometry)
{
    auto imported = create_test_transaction (m_book, m_bank, m_currency,
                                              100, NREC, TRUE);
    auto high_score = create_test_transaction (m_book, m_bank, m_currency,
                                               100, NREC, FALSE);
    auto low_score = create_test_transaction (m_book, m_bank, m_currency,
                                              100, NREC, FALSE);
    xaccTransBeginEdit (low_score.transaction);
    xaccTransSetDescription (low_score.transaction, "different candidate");
    xaccTransCommitEdit (low_score.transaction);
    auto trans_info = gnc_import_TransInfo_new (imported.transaction, m_bank);
    split_find_match (trans_info, high_score.split, 0, 4, 14, 0.0);
    split_find_match (trans_info, low_score.split, 0, 4, 14, 0.0);
    auto high_match = find_match_for_split (trans_info, high_score.split);
    auto low_match = find_match_for_split (trans_info, low_score.split);
    ASSERT_NE (high_match, nullptr);
    ASSERT_NE (low_match, nullptr);
    auto pending_matches = gnc_import_PendingMatches_new ();
    MatchPickerResult result;

    gnc_import_match_picker_run (nullptr, trans_info, pending_matches,
                                 match_picker_done, &result);

    auto window = find_buildable_window ("match_picker_dialog");
    ASSERT_NE (window, nullptr);
    auto scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "matched_view"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (scroller));
    auto view = GTK_COLUMN_VIEW (gtk_scrolled_window_get_child (scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (view));
    auto columns = gtk_column_view_get_columns (view);
    auto confidence_column = GTK_COLUMN_VIEW_COLUMN (
        g_list_model_get_item (columns, 0));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW_COLUMN (confidence_column));

    /* Give the cell spare horizontal room. The picture must keep the score
     * generator's natural dimensions instead of scaling into the column. */
    gtk_column_view_column_set_fixed_width (confidence_column, 320);
    gtk_window_present (window);
    ASSERT_TRUE (spin_until_frame (GTK_WIDGET (window)));

    std::vector<GtkWidget*> pictures;
    collect_descendants_of_type (GTK_WIDGET (view), GTK_TYPE_PICTURE, pictures);
    pictures.erase (std::remove_if (pictures.begin (), pictures.end (),
                                    [] (GtkWidget *picture) {
                                        return !gtk_widget_get_mapped (picture);
                                    }), pictures.end ());
    ASSERT_EQ (pictures.size (), 2u);

    std::vector<int> expected_widths {
        7 * gnc_import_MatchInfo_get_probability (high_match) + 1,
        7 * gnc_import_MatchInfo_get_probability (low_match) + 1};
    std::sort (expected_widths.begin (), expected_widths.end ());
    std::vector<int> actual_widths;
    for (auto picture_widget : pictures)
    {
        auto picture = GTK_PICTURE (picture_widget);
        auto paintable = gtk_picture_get_paintable (picture);
        ASSERT_NE (paintable, nullptr);
        EXPECT_FALSE (gtk_picture_get_can_shrink (picture));
        EXPECT_EQ (gtk_picture_get_content_fit (picture),
                   GTK_CONTENT_FIT_SCALE_DOWN);
        EXPECT_EQ (gtk_widget_get_halign (picture_widget), GTK_ALIGN_START);
        EXPECT_EQ (gtk_widget_get_valign (picture_widget), GTK_ALIGN_CENTER);
        const auto width = gdk_paintable_get_intrinsic_width (paintable);
        const auto height = gdk_paintable_get_intrinsic_height (paintable);
        EXPECT_EQ (height, 15);
        EXPECT_EQ (gtk_widget_get_width (picture_widget), width);
        EXPECT_EQ (gtk_widget_get_height (picture_widget), height);
        actual_widths.push_back (width);
    }
    std::sort (actual_widths.begin (), actual_widths.end ());
    EXPECT_EQ (actual_widths, expected_widths);

    if (auto snapshot_path = g_getenv ("GNC_TEST_IMPORT_MATCH_PICKER_SNAPSHOT"))
    {
        ASSERT_TRUE (g_path_is_absolute (snapshot_path));
        ASSERT_TRUE (spin_until_frame (GTK_WIDGET (window)));
        EXPECT_TRUE (save_matcher_snapshot (GTK_WIDGET (window), snapshot_path));
    }

    auto ok_button = gtk_window_get_default_widget (window);
    ASSERT_TRUE (GTK_IS_BUTTON (ok_button));
    g_signal_emit_by_name (ok_button, "clicked");
    EXPECT_EQ (result.calls, 1u);
    g_object_unref (confidence_column);
    g_object_unref (window);
    gnc_import_PendingMatches_delete (pending_matches);
    gnc_import_TransInfo_delete (trans_info);
}

TEST_F(ImportMatcherTest, match_picker_external_destroy_finishes_once)
{
    auto imported = create_test_transaction (m_book, m_bank, m_currency,
                                             100, NREC, TRUE);
    auto candidate = create_test_transaction (m_book, m_bank, m_currency,
                                              100, NREC, FALSE);
    auto trans_info = gnc_import_TransInfo_new (imported.transaction, m_bank);
    split_find_match (trans_info, candidate.split, 0, 4, 14, 0.0);
    auto pending_matches = gnc_import_PendingMatches_new ();
    MatchPickerResult result;

    gnc_import_match_picker_run (nullptr, trans_info, pending_matches,
                                 match_picker_done, &result);

    auto window = find_buildable_window ("match_picker_dialog");
    ASSERT_NE (window, nullptr);
    auto match_scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "matched_view"));
    auto downloaded_scroller = GTK_SCROLLED_WINDOW (find_buildable_widget (
        GTK_WIDGET (window), "download_view"));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (match_scroller));
    ASSERT_TRUE (GTK_IS_SCROLLED_WINDOW (downloaded_scroller));
    auto match_view = GTK_COLUMN_VIEW (
        gtk_scrolled_window_get_child (match_scroller));
    auto downloaded_view = GTK_COLUMN_VIEW (
        gtk_scrolled_window_get_child (downloaded_scroller));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (match_view));
    ASSERT_TRUE (GTK_IS_COLUMN_VIEW (downloaded_view));
    auto match_selection = GTK_SINGLE_SELECTION (
        gtk_column_view_get_model (match_view));
    auto downloaded_selection = GTK_SINGLE_SELECTION (
        gtk_column_view_get_model (downloaded_view));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (match_selection));
    ASSERT_TRUE (GTK_IS_SINGLE_SELECTION (downloaded_selection));
    ASSERT_GT (g_list_model_get_n_items (G_LIST_MODEL (match_selection)), 0u);
    ASSERT_GT (g_list_model_get_n_items (G_LIST_MODEL (downloaded_selection)), 0u);
    g_object_ref (match_selection);
    g_object_ref (downloaded_selection);
    GWeakRef match_selection_ref;
    GWeakRef downloaded_selection_ref;
    GWeakRef window_ref;
    g_weak_ref_init (&match_selection_ref, G_OBJECT (match_selection));
    g_weak_ref_init (&downloaded_selection_ref,
                     G_OBJECT (downloaded_selection));
    g_weak_ref_init (&window_ref, G_OBJECT (window));

    gtk_window_destroy (window);
    EXPECT_EQ (result.calls, 0u);
    g_object_unref (window);

    EXPECT_TRUE (weak_ref_was_finalized (&window_ref));
    EXPECT_EQ (result.calls, 1u);
    gtk_single_selection_set_selected (match_selection, 0);
    gtk_single_selection_set_selected (downloaded_selection,
                                       GTK_INVALID_LIST_POSITION);
    EXPECT_EQ (result.calls, 1u);
    g_object_unref (downloaded_selection);
    g_object_unref (match_selection);
    EXPECT_TRUE (weak_ref_was_finalized (&downloaded_selection_ref));
    EXPECT_TRUE (weak_ref_was_finalized (&match_selection_ref));
    gnc_import_PendingMatches_delete (pending_matches);
    gnc_import_TransInfo_delete (trans_info);
}

TEST_F(ImportMatcherTest, matcher_then_ofx_cancel_coalesces_and_cleans_once)
{
    run_matcher_ofx_cancel_order (m_book, TRUE);
}

TEST_F(ImportMatcherTest, ofx_then_matcher_cancel_coalesces_and_cleans_once)
{
    run_matcher_ofx_cancel_order (m_book, FALSE);
}

TEST_F(ImportMatcherTest, ofx_immediate_cleanup_uses_product_lifecycle)
{
    auto application = g_application_new (nullptr, G_APPLICATION_NON_UNIQUE);
    OfxLifecycleMetrics metrics;
    OfxLifecyclePayload *payload = nullptr;
    auto lifecycle = create_ofx_lifecycle (m_book, application, &metrics,
                                           &payload);
    ASSERT_NE (lifecycle, nullptr);
    add_open_transaction (m_book, payload);

    EXPECT_TRUE (gnc_ofx_import_lifecycle_request (lifecycle));
    EXPECT_EQ (metrics.metadata_cleanup_calls, 1u);
    EXPECT_EQ (metrics.payload_destroy_calls, 1u);
    EXPECT_EQ (metrics.result,
               GNC_IMPORT_OPERATION_TEARDOWN_MUTATION_ALLOWED);
    g_object_unref (application);
}

TEST_F(ImportMatcherTest, parent_abort_keeps_payload_until_async_state_releases)
{
    auto application = g_application_new (nullptr, G_APPLICATION_NON_UNIQUE);
    OfxLifecycleMetrics metrics;
    OfxLifecyclePayload *payload = nullptr;
    auto lifecycle = create_ofx_lifecycle (m_book, application, &metrics,
                                           &payload);
    ASSERT_NE (lifecycle, nullptr);
    auto state = gnc_ofx_import_async_state_new (lifecycle);
    ASSERT_NE (state, nullptr);

    EXPECT_TRUE (gnc_ofx_import_async_state_request_teardown (state));
    EXPECT_FALSE (gnc_ofx_import_async_state_is_active (state));
    EXPECT_EQ (metrics.metadata_cleanup_calls, 1u);
    EXPECT_EQ (metrics.payload_destroy_calls, 0u);
    gnc_ofx_import_async_state_unref (state);
    EXPECT_EQ (metrics.payload_destroy_calls, 1u);
    g_object_unref (application);
}

TEST_F(ImportMatcherTest, parent_abort_disconnects_reconcile_before_window_destroy)
{
    auto application = g_application_new (nullptr, G_APPLICATION_NON_UNIQUE);
    OfxLifecycleMetrics metrics;
    OfxLifecyclePayload *payload = nullptr;
    auto lifecycle = create_ofx_lifecycle (m_book, application, &metrics,
                                           &payload);
    ASSERT_NE (lifecycle, nullptr);
    auto window = gtk_window_new ();
    g_object_ref_sink (window);
    ASSERT_TRUE (gnc_ofx_import_lifecycle_connect_destroy (
        lifecycle, G_OBJECT (window), reconcile_continuation_called,
        &metrics));

    EXPECT_TRUE (gnc_ofx_import_lifecycle_request (lifecycle));
    EXPECT_EQ (metrics.metadata_cleanup_calls, 1u);
    EXPECT_EQ (metrics.payload_destroy_calls, 1u);
    gtk_window_destroy (GTK_WINDOW (window));
    EXPECT_EQ (metrics.reconcile_calls, 0u);
    g_object_unref (window);
    g_object_unref (application);
}

TEST_F(ImportMatcherTest, shutdown_destroys_retry_source_and_transfers_book_ownership)
{
    auto application = g_application_new (nullptr, G_APPLICATION_NON_UNIQUE);
    OfxLifecycleMetrics metrics;
    OfxLifecyclePayload *payload = nullptr;
    auto lifecycle = create_ofx_lifecycle (m_book, application, &metrics,
                                           &payload);
    ASSERT_NE (lifecycle, nullptr);
    auto raw_transaction = add_open_transaction (m_book, payload);

    auto save_lease = qof_session_operation_lease_acquire_for (
        gnc_get_current_session (), QOF_SESSION_OPERATION_SAVE);
    ASSERT_NE (save_lease, nullptr);
    /* No cancel/request precedes shutdown: the production owner must still
     * terminalize the multi-turn workflow and release its application hold. */
    g_signal_emit_by_name (application, "shutdown");

    EXPECT_EQ (metrics.metadata_cleanup_calls, 1u);
    EXPECT_EQ (metrics.payload_destroy_calls, 1u);
    EXPECT_EQ (metrics.result,
               GNC_IMPORT_OPERATION_TEARDOWN_BOOK_SHUTDOWN);
    ASSERT_NE (raw_transaction, nullptr);
    EXPECT_TRUE (xaccTransIsOpen (raw_transaction));
    EXPECT_EQ (qof_instance_get_book (QOF_INSTANCE (raw_transaction)), m_book);
    EXPECT_EQ (qof_collection_lookup_entity (
                   qof_book_get_collection (m_book, GNC_ID_TRANS),
                   qof_instance_get_guid (QOF_INSTANCE (raw_transaction))),
               QOF_INSTANCE (raw_transaction));
    qof_session_operation_lease_release (save_lease);

    /* BOOK_SHUTDOWN deliberately leaves the open object owned by QofBook. The
     * live-fixture test cleans it under a fresh lease instead of destroying the
     * book, proving that the owner released only non-owning references. */
    auto cleanup_lease = qof_session_operation_lease_acquire_for (
        gnc_get_current_session (), QOF_SESSION_OPERATION_IMPORT);
    ASSERT_NE (cleanup_lease, nullptr);
    xaccTransDestroy (raw_transaction);
    xaccTransCommitEdit (raw_transaction);
    qof_session_operation_lease_release (cleanup_lease);
    g_object_unref (application);
}

TEST_F(ImportMatcherTest, pending_timeout_shutdown_completes_once_and_cancels_retry)
{
    auto application = g_application_new (nullptr, G_APPLICATION_NON_UNIQUE);
    OfxLifecycleMetrics metrics;
    OfxLifecyclePayload *payload = nullptr;
    auto lifecycle = create_ofx_lifecycle (m_book, application, &metrics,
                                           &payload);
    ASSERT_NE (lifecycle, nullptr);
    auto raw_transaction = add_open_transaction (m_book, payload);

    auto save_lease = qof_session_operation_lease_acquire_for (
        gnc_get_current_session (), QOF_SESSION_OPERATION_SAVE);
    ASSERT_NE (save_lease, nullptr);
    EXPECT_FALSE (gnc_ofx_import_lifecycle_request (lifecycle));
    EXPECT_TRUE (gnc_import_operation_teardown_has_pending_retry (
        gnc_ofx_import_lifecycle_get_teardown (lifecycle)));
    EXPECT_EQ (metrics.metadata_cleanup_calls, 0u);
    EXPECT_EQ (metrics.payload_destroy_calls, 0u);

    g_signal_emit_by_name (application, "shutdown");

    EXPECT_EQ (metrics.metadata_cleanup_calls, 1u);
    EXPECT_EQ (metrics.payload_destroy_calls, 1u);
    EXPECT_EQ (metrics.result,
               GNC_IMPORT_OPERATION_TEARDOWN_BOOK_SHUTDOWN);
    ASSERT_NE (raw_transaction, nullptr);
    EXPECT_TRUE (xaccTransIsOpen (raw_transaction));
    for (guint turn = 0; turn < 4; ++turn)
        g_main_context_iteration (nullptr, FALSE);
    EXPECT_EQ (metrics.metadata_cleanup_calls, 1u);
    EXPECT_EQ (metrics.payload_destroy_calls, 1u);

    qof_session_operation_lease_release (save_lease);
    auto cleanup_lease = qof_session_operation_lease_acquire_for (
        gnc_get_current_session (), QOF_SESSION_OPERATION_IMPORT);
    ASSERT_NE (cleanup_lease, nullptr);
    xaccTransDestroy (raw_transaction);
    xaccTransCommitEdit (raw_transaction);
    qof_session_operation_lease_release (cleanup_lease);
    g_object_unref (application);
}

TEST_F(ImportMatcherTest, test_noisy_match)
{
    auto found = gnc_import_select_account(nullptr, "BankUSD", FALSE, nullptr,
                                           nullptr, ACCT_TYPE_NONE, nullptr,
                                           nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("Bank", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_match_with_subaccounts)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocks", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("Stocks", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_match)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksHPE", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("HPE", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_match_trailing_noise)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksHPEUSD", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("HPE", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_no_match)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksINTC", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_STOCK,
                                           nullptr, nullptr);
    ASSERT_EQ(nullptr, found);
}

TEST_F(ImportMatcherTest, test_subaccount_match_trailing_space)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksMSFT ", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("MSFT", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_match_trim_trailing_space)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksMSFT", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("MSFT", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_match_internal_space)
{
    auto found = gnc_import_select_account(nullptr, "BrokerCash Management",
                                           FALSE, nullptr, nullptr,
                                           ACCT_TYPE_NONE, nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("Cash Management", xaccAccountGetName(found));
}
