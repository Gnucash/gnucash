/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>

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
gboolean display_available;
GtkWidget *window_to_destroy_on_price_event;
gboolean destroy_window_on_price_event;

void
destroy_window_on_price_event_cb (QofInstance *, QofEventId event_type,
                                  gpointer, gpointer)
{
    if (!destroy_window_on_price_event ||
        !(event_type & (QOF_EVENT_MODIFY | QOF_EVENT_DESTROY)))
        return;
    destroy_window_on_price_event = FALSE;
    auto window = window_to_destroy_on_price_event;
    window_to_destroy_on_price_event = nullptr;
    gtk_widget_destroy (window);
}

GtkWidget *
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

GtkWidget *
find_transient_dialog (GtkWindow *parent, gboolean message)
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
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

GtkWidget *
find_price_window ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
        if (g_strcmp0 (gtk_widget_get_name (GTK_WIDGET (node->data)),
                       "gnc-id-price-edit") == 0)
        {
            g_assert_null (result);
            result = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return result;
}

void
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
    const time64 dates[] = {1000086400, 1000172800};
    for (auto date : dates)
    {
        auto price = gnc_price_create (book);
        gnc_price_set_commodity (price, commodity);
        gnc_price_set_currency (price, currency);
        gnc_price_set_time64 (price, date);
        gnc_price_set_source (price, PRICE_SOURCE_USER_PRICE);
        gnc_price_set_value (price, gnc_numeric_create (42, 1));
        g_assert_true (gnc_pricedb_add_price (gnc_pricedb_get_db (book), price));
        gnc_price_unref (price);
    }
    *commodity_out = commodity;
}

void
test_confirmed_removal_is_async_and_single (void)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    auto session = qof_session_new (book);
    gnc_set_current_session (session);
    gnc_commodity *commodity = nullptr;
    setup_price (book, &commodity);
    auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (owner);
    gnc_prices_dialog (owner);
    auto price_window = find_price_window ();
    g_assert_nonnull (price_window);
    auto remove_old = find_buildable (price_window, "remove_old_button");
    g_assert_true (GTK_IS_BUTTON (remove_old));
    gtk_button_clicked (GTK_BUTTON (remove_old));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), FALSE);
    g_assert_true (GTK_IS_DIALOG (removal));

    auto user_source = find_buildable (removal, "checkbutton_user");
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (user_source), TRUE);
    auto view = find_buildable (removal, "commodty_treeview");
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    g_assert_true (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)),
                                    path);
    gtk_tree_path_free (path);
    auto count = gnc_pricedb_num_prices (gnc_pricedb_get_db (book), commodity);
    g_assert_cmpint (count, ==, 2);

    /* Returning from Apply must not enter a nested loop; confirmation remains
       pending while the caller continues to run this test. */
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    auto confirmation = find_transient_dialog (GTK_WINDOW (removal), TRUE);
    g_assert_nonnull (confirmation);
    g_object_ref (confirmation);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    g_assert_cmpint (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), ==, 1);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    g_assert_cmpint (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), ==, 1);
    g_object_unref (confirmation);

    gtk_widget_destroy (price_window);
    gtk_widget_destroy (owner);
    gnc_clear_current_session ();
}

void
test_parent_destroy_and_late_confirmation_do_not_mutate (void)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    auto session = qof_session_new (book);
    gnc_set_current_session (session);
    gnc_commodity *commodity = nullptr;
    setup_price (book, &commodity);
    auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (owner);
    gnc_prices_dialog (owner);
    auto price_window = find_price_window ();
    auto remove_old = find_buildable (price_window, "remove_old_button");
    gtk_button_clicked (GTK_BUTTON (remove_old));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), FALSE);
    auto user_source = find_buildable (removal, "checkbutton_user");
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (user_source), TRUE);
    auto view = find_buildable (removal, "commodty_treeview");
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    g_assert_true (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)),
                                    path);
    gtk_tree_path_free (path);
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    auto confirmation = find_transient_dialog (GTK_WINDOW (removal), TRUE);
    g_assert_nonnull (confirmation);
    g_object_ref (confirmation);

    gtk_widget_destroy (price_window);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    g_assert_cmpint (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), ==, 2);
    g_object_unref (confirmation);
    gtk_widget_destroy (owner);
    gnc_clear_current_session ();
}

void
test_session_change_before_confirmation_does_not_mutate (void)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    auto session = qof_session_new (book);
    gnc_set_current_session (session);
    gnc_commodity *commodity = nullptr;
    setup_price (book, &commodity);
    auto replacement_book = qof_book_new ();
    auto replacement_session = qof_session_new (replacement_book);
    auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (owner);
    gnc_prices_dialog (owner);
    auto price_window = find_price_window ();
    gtk_button_clicked (GTK_BUTTON (find_buildable (price_window,
                                                    "remove_old_button")));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), FALSE);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (find_buildable (removal, "checkbutton_user")), TRUE);
    auto view = find_buildable (removal, "commodty_treeview");
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    g_assert_true (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)),
                                    path);
    gtk_tree_path_free (path);
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    auto confirmation = find_transient_dialog (GTK_WINDOW (removal), TRUE);
    g_assert_nonnull (confirmation);

    gnc_set_current_session (replacement_session);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    g_assert_cmpint (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), ==, 2);
    gtk_widget_destroy (price_window);
    gtk_widget_destroy (owner);
    gnc_clear_current_session ();
    qof_session_destroy (session);
}

void
test_late_response_after_removal_dialog_destroy (void)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    auto session = qof_session_new (book);
    gnc_set_current_session (session);
    gnc_commodity *commodity = nullptr;
    setup_price (book, &commodity);
    auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (owner);
    gnc_prices_dialog (owner);
    auto price_window = find_price_window ();
    gtk_button_clicked (GTK_BUTTON (find_buildable (price_window,
                                                    "remove_old_button")));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), FALSE);
    g_object_ref (removal);
    gtk_widget_destroy (removal);
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    g_assert_cmpint (gnc_pricedb_num_prices (gnc_pricedb_get_db (book),
                                             commodity), ==, 2);
    g_object_unref (removal);
    gtk_widget_destroy (price_window);
    gtk_widget_destroy (owner);
    gnc_clear_current_session ();
}

void
test_owner_destroyed_by_price_removal_event (void)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    auto session = qof_session_new (book);
    gnc_set_current_session (session);
    gnc_commodity *commodity = nullptr;
    setup_price (book, &commodity);
    auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (owner);
    gnc_prices_dialog (owner);
    auto price_window = find_price_window ();
    gtk_button_clicked (GTK_BUTTON (find_buildable (price_window,
                                                    "remove_old_button")));
    auto removal = find_transient_dialog (GTK_WINDOW (price_window), FALSE);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (find_buildable (removal, "checkbutton_user")), TRUE);
    auto view = find_buildable (removal, "commodty_treeview");
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    g_assert_true (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)),
                                    path);
    gtk_tree_path_free (path);
    gtk_dialog_response (GTK_DIALOG (removal), GTK_RESPONSE_APPLY);
    auto confirmation = find_transient_dialog (GTK_WINDOW (removal), TRUE);
    g_assert_nonnull (confirmation);

    window_to_destroy_on_price_event = price_window;
    destroy_window_on_price_event = TRUE;
    auto event_handler = qof_event_register_handler (
        destroy_window_on_price_event_cb, nullptr);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    qof_event_unregister_handler (event_handler);
    destroy_window_on_price_event = FALSE;
    window_to_destroy_on_price_event = nullptr;
    g_assert_null (find_price_window ());
    gtk_widget_destroy (owner);
    gnc_clear_current_session ();
}
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_test_add_func ("/gnome/price-remove/confirmed-async-single",
                     test_confirmed_removal_is_async_and_single);
    g_test_add_func ("/gnome/price-remove/parent-destroy-late-response",
                     test_parent_destroy_and_late_confirmation_do_not_mutate);
    g_test_add_func ("/gnome/price-remove/session-change",
                     test_session_change_before_confirmation_does_not_mutate);
    g_test_add_func ("/gnome/price-remove/removal-destroy-late-response",
                     test_late_response_after_removal_dialog_destroy);
    g_test_add_func ("/gnome/price-remove/owner-destroyed-by-price-event",
                     test_owner_destroyed_by_price_removal_event);
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
