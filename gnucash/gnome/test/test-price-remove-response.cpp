/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include "test-logging.hpp"
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"

#include "gnc-component-manager.h"
#include "cashobjects.h"
#include "gnc-gsettings.h"
#include "gnc-pricedb.h"
#include "gnc-session.h"
#include "gnc-commodity.h"
#include "qofevent.h"
#include "qof.h"

extern "C" void gnc_prices_dialog (GtkWidget *parent);

namespace
{
struct PriceEventState
{
    GtkWidget *window{};
    bool destroy_window{};
};

static void
destroy_window_on_price_event_cb (QofInstance *, QofEventId event_type,
                                  gpointer user_data, gpointer)
{
    auto state = static_cast<PriceEventState *> (user_data);
    if (!state->destroy_window ||
        !(event_type & (QOF_EVENT_MODIFY | QOF_EVENT_DESTROY)))
        return;
    state->destroy_window = false;
    gtk_widget_destroy (state->window);
    state->window = nullptr;
}

static GtkWidget *
find_buildable (GtkWidget *root, const char *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_buildable (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return result;
}

static GtkWidget *
find_transient_dialog (GtkWindow *parent, bool message)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) == parent &&
            (!message || GTK_IS_MESSAGE_DIALOG (widget)))
        {
            EXPECT_EQ (result, nullptr);
            if (result)
            {
                g_list_free (windows);
                return nullptr;
            }
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

static GtkWidget *
find_price_window ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
        if (g_strcmp0 (gtk_widget_get_name (GTK_WIDGET (node->data)),
                       "gnc-id-price-edit") == 0)
        {
            EXPECT_EQ (result, nullptr);
            if (result)
            {
                g_list_free (windows);
                return nullptr;
            }
            result = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return result;
}

static bool
setup_price (QofBook *book, gnc_commodity **commodity_out)
{
    auto table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (table, "NASDAQ", book);
    auto commodity = gnc_commodity_new (book, "Response Test", "NASDAQ",
                                        "RSP", nullptr, 1000);
    commodity = gnc_commodity_table_insert (table, commodity);
    auto currency = gnc_commodity_table_lookup (table, "CURRENCY", "USD");
    if (!currency)
    {
        gnc_commodity_table_add_namespace (table, "CURRENCY", book);
        currency = gnc_commodity_new (book, "US Dollar", "CURRENCY", "USD",
                                      "$", 100);
        currency = gnc_commodity_table_insert (table, currency);
    }
    /* Keep-last-week intentionally retains one price per ISO week. Use two
     * different days within the same week so removal has one observable
     * deletion while both fixture prices remain older than the cutoff. */
    const time64 dates[] = {1000259200, 1000345600};
    for (auto date : dates)
    {
        auto price = gnc_price_create (book);
        gnc_price_set_commodity (price, commodity);
        gnc_price_set_currency (price, currency);
        gnc_price_set_time64 (price, date);
        gnc_price_set_source (price, PRICE_SOURCE_USER_PRICE);
        gnc_price_set_value (price, gnc_numeric_create (42, 1));
        auto added = gnc_pricedb_add_price (gnc_pricedb_get_db (book), price);
        gnc_price_unref (price);
        if (!added)
        {
            EXPECT_TRUE (added);
            return false;
        }
    }
    *commodity_out = commodity;
    return true;
}

class PriceRemoveResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        book = qof_book_new ();
        session = qof_session_new (book);
        gnc_set_current_session (session);
        ASSERT_TRUE (setup_price (book, &commodity));
        owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (owner);
        gtk_widget_realize (owner);
        gnc_prices_dialog (owner);
        price_window = find_price_window ();
        ASSERT_NE (price_window, nullptr);
        g_object_ref (price_window);
    }

    void TearDown () override
    {
        if (event_handler)
            qof_event_unregister_handler (event_handler);
        if (price_window)
        {
            gtk_widget_destroy (price_window);
            g_object_unref (price_window);
        }
        for (auto widget : retained_widgets)
            g_object_unref (widget);
        if (owner)
        {
            gtk_widget_destroy (owner);
            g_object_unref (owner);
        }
        GnomeResponseTest::TearDown ();
        gnc_clear_current_session ();
        if (replacement_session)
        {
            if (session_switched)
                qof_session_destroy (session);
            else
                qof_session_destroy (replacement_session);
        }
        session = nullptr;
        replacement_session = nullptr;
    }

    QofBook *book{};
    QofSession *session{};
    QofSession *replacement_session{};
    bool session_switched{};
    gnc_commodity *commodity{};
    GtkWidget *owner{};
    GtkWidget *price_window{};
    gulong event_handler{};
    PriceEventState event_state{};
    std::vector<GtkWidget *> retained_widgets;

    void retain_widget (GtkWidget *widget)
    {
        g_object_ref (widget);
        retained_widgets.push_back (widget);
    }
};

TEST_F (PriceRemoveResponseTest, PriceRemoveConfirmedAsyncSingle)
{
    ASSERT_NE (price_window, nullptr);
    auto remove_old = find_buildable (price_window, "remove_old_button");
    ASSERT_TRUE (GTK_IS_BUTTON (remove_old));
    gtk_button_clicked (GTK_BUTTON (remove_old));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), false);
    ASSERT_TRUE (GTK_IS_DIALOG (removal));

    auto user_source = find_buildable (removal, "checkbutton_user");
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (user_source), true);
    auto view = find_buildable (removal, "commodty_treeview");
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)),
                                    path);
    gtk_tree_path_free (path);
    auto count = gnc_pricedb_num_prices (gnc_pricedb_get_db (book), commodity);
    EXPECT_EQ (count, 2);

    /* Returning from Apply must not enter a nested loop; confirmation remains
       pending while the caller continues to run this test. */
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    auto confirmation = find_transient_dialog (GTK_WINDOW (removal), true);
    ASSERT_NE (confirmation, nullptr);
    retain_widget (confirmation);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    EXPECT_EQ (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), 1);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    EXPECT_EQ (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), 1);

}

TEST_F (PriceRemoveResponseTest, PriceRemoveParentDestroyLateResponse)
{
    auto remove_old = find_buildable (price_window, "remove_old_button");
    gtk_button_clicked (GTK_BUTTON (remove_old));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), false);
    auto user_source = find_buildable (removal, "checkbutton_user");
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (user_source), true);
    auto view = find_buildable (removal, "commodty_treeview");
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)),
                                    path);
    gtk_tree_path_free (path);
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    auto confirmation = find_transient_dialog (GTK_WINDOW (removal), true);
    ASSERT_NE (confirmation, nullptr);
    retain_widget (confirmation);

    gtk_widget_destroy (price_window);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    EXPECT_EQ (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), 2);
}

TEST_F (PriceRemoveResponseTest, PriceRemoveSessionChange)
{
    replacement_session = qof_session_new (qof_book_new ());
    gtk_button_clicked (GTK_BUTTON (find_buildable (price_window,
                                                    "remove_old_button")));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), false);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (find_buildable (removal, "checkbutton_user")), true);
    auto view = find_buildable (removal, "commodty_treeview");
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)),
                                    path);
    gtk_tree_path_free (path);
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    auto confirmation = find_transient_dialog (GTK_WINDOW (removal), true);
    ASSERT_NE (confirmation, nullptr);

    gnc_set_current_session (replacement_session);
    session_switched = true;
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    EXPECT_EQ (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), 2);
}

TEST_F (PriceRemoveResponseTest, PriceRemoveDialogDestroyLateResponse)
{
    gtk_button_clicked (GTK_BUTTON (find_buildable (price_window,
                                                    "remove_old_button")));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), false);
    retain_widget (removal);
    gtk_widget_destroy (removal);
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    EXPECT_EQ (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                       commodity), 2);
}

TEST_F (PriceRemoveResponseTest, PriceRemovalEventMayDestroyOwner)
{
    gtk_button_clicked (GTK_BUTTON (find_buildable (price_window,
                                                    "remove_old_button")));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), false);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (find_buildable (removal, "checkbutton_user")), true);
    auto view = find_buildable (removal, "commodty_treeview");
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)),
                                    path);
    gtk_tree_path_free (path);
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    auto confirmation = find_transient_dialog (GTK_WINDOW (removal), true);
    ASSERT_NE (confirmation, nullptr);

    event_state = {price_window, true};
    event_handler = qof_event_register_handler (
        destroy_window_on_price_event_cb, &event_state);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    qof_event_unregister_handler (event_handler);
    event_handler = 0;
    EXPECT_EQ (find_price_window (), nullptr);
}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed; GUI tests require a display.\n");
        return 1;
    }
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
