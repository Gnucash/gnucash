/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-commodity.h"
#include "gnc-currency-edit.h"
#include "gnc-engine.h"
#include "gnc-session.h"

static void
model_finalized (gpointer data, GObject *object)
{
    gboolean *finalized = data;

    *finalized = TRUE;
    (void)object;
}

static GtkListView *
currency_list_view (GtkWidget *widget)
{
    GtkMenuButton *menu_button = GTK_MENU_BUTTON (
        gtk_widget_get_last_child (widget));
    GtkPopover *popover = GTK_POPOVER (
        gtk_menu_button_get_popover (menu_button));
    GtkScrolledWindow *scroller = GTK_SCROLLED_WINDOW (
        gtk_popover_get_child (popover));

    return GTK_LIST_VIEW (gtk_scrolled_window_get_child (scroller));
}

static GtkEventControllerKey *
entry_key_controller (GtkWidget *entry)
{
    GListModel *controllers = gtk_widget_observe_controllers (entry);
    GtkEventControllerKey *key_controller = NULL;

    for (guint index = 0;
         index < g_list_model_get_n_items (controllers) && !key_controller;
         index++)
    {
        GObject *controller = g_list_model_get_item (controllers, index);

        if (GTK_IS_EVENT_CONTROLLER_KEY (controller))
            key_controller = GTK_EVENT_CONTROLLER_KEY (controller);
        else
            g_object_unref (controller);
    }
    g_object_unref (controllers);
    return key_controller;
}

static void
destroy_currency_edit_on_changed (GNCCurrencyEdit *currency_edit,
                                  gpointer user_data)
{
    gboolean *destroyed = user_data;

    *destroyed = TRUE;
    g_object_unref (currency_edit);
}

static void
test_model_is_complete_before_selection (void)
{
    GLogLevelFlags old_fatal_mask;
    GtkWidget *widget;
    GtkWidget *entry;
    GtkListView *list_view;
    GtkSingleSelection *selection;
    GObject *item;
    gnc_commodity *currency;
    gnc_commodity *preview_currency;

    old_fatal_mask = g_log_set_always_fatal (G_LOG_FATAL_MASK |
                                              G_LOG_LEVEL_CRITICAL);
    widget = gnc_currency_edit_new ();
    g_object_ref_sink (widget);
    entry = gtk_widget_get_first_child (widget);
    list_view = currency_list_view (widget);
    selection = GTK_SINGLE_SELECTION (gtk_list_view_get_model (list_view));

    g_assert_true (GTK_IS_ENTRY (entry));
    g_assert_cmpuint (gtk_single_selection_get_selected (selection), ==,
                      GTK_INVALID_LIST_POSITION);
    g_assert_cmpstr (gtk_editable_get_text (GTK_EDITABLE (entry)), ==, "");

    item = g_list_model_get_item (G_LIST_MODEL (selection), 0);
    g_signal_emit_by_name (list_view, "activate", 0);
    currency = gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT (widget));
    g_assert_nonnull (currency);
    g_assert_cmpstr (gnc_commodity_get_printname (currency), ==,
                     gtk_string_object_get_string (GTK_STRING_OBJECT (item)));
    g_assert_cmpstr (gtk_editable_get_text (GTK_EDITABLE (entry)), ==,
                     gnc_commodity_get_printname (currency));

    gtk_single_selection_set_selected (selection, 1);
    preview_currency = gnc_currency_edit_get_currency (
        GNC_CURRENCY_EDIT (widget));
    g_assert_true (preview_currency == currency);
    g_signal_emit_by_name (list_view, "activate", 1);
    g_assert_true (gnc_currency_edit_get_currency (
        GNC_CURRENCY_EDIT (widget)) != currency);

    g_object_unref (item);
    gnc_currency_edit_clear_display (GNC_CURRENCY_EDIT (widget));
    g_assert_cmpuint (gtk_single_selection_get_selected (selection), ==,
                      GTK_INVALID_LIST_POSITION);
    g_assert_cmpstr (gtk_editable_get_text (GTK_EDITABLE (entry)), ==, "");
    g_object_unref (widget);
    g_log_set_always_fatal (old_fatal_mask);
}

static void
test_changed_handler_may_destroy_widget (void)
{
    GtkWidget *widget = gnc_currency_edit_new ();
    GtkListView *list_view;
    gboolean destroyed = FALSE;

    g_object_ref_sink (widget);
    list_view = currency_list_view (widget);
    g_signal_connect (widget, "changed",
                      G_CALLBACK (destroy_currency_edit_on_changed), &destroyed);

    g_signal_emit_by_name (list_view, "activate", 0);
    g_assert_true (destroyed);
}

static void
count_changed (GNCCurrencyEdit *currency_edit, gpointer user_data)
{
    guint *count = user_data;

    (*count)++;
    (void)currency_edit;
}

static void
test_keyboard_popup_does_not_commit (void)
{
    GtkWindow *window = GTK_WINDOW (gtk_window_new ());
    GtkWidget *widget = gnc_currency_edit_new ();
    GtkWidget *entry;
    GtkMenuButton *menu_button;
    GtkEventControllerKey *key_controller;
    GtkPopover *popover;
    gboolean handled = FALSE;
    guint changed_count = 0;

    g_object_ref_sink (window);
    g_object_ref_sink (widget);
    gtk_window_set_child (window, widget);
    gtk_widget_realize (GTK_WIDGET (window));
    entry = gtk_widget_get_first_child (widget);
    menu_button = GTK_MENU_BUTTON (gtk_widget_get_last_child (widget));
    popover = gtk_menu_button_get_popover (menu_button);
    key_controller = entry_key_controller (entry);
    g_assert_nonnull (key_controller);
    g_signal_connect (widget, "changed", G_CALLBACK (count_changed),
                      &changed_count);

    g_signal_emit_by_name (key_controller, "key-pressed", GDK_KEY_Down, 0,
                           GDK_ALT_MASK, &handled);
    g_assert_true (handled);
    g_assert_true (gtk_menu_button_get_active (menu_button));
    g_assert_true (gtk_widget_get_visible (GTK_WIDGET (popover)));
    g_assert_cmpuint (changed_count, ==, 0);
    g_assert_cmpstr (gtk_editable_get_text (GTK_EDITABLE (entry)), ==, "");

    gtk_menu_button_popdown (menu_button);
    g_assert_false (gtk_menu_button_get_active (menu_button));
    g_assert_false (gtk_widget_get_visible (GTK_WIDGET (popover)));
    g_assert_cmpuint (changed_count, ==, 0);
    g_assert_cmpstr (gtk_editable_get_text (GTK_EDITABLE (entry)), ==, "");

    g_object_unref (key_controller);
    gtk_window_destroy (window);
    g_object_unref (widget);
    g_object_unref (window);
}

static void
test_partial_text_does_not_retain_model (void)
{
    GtkWidget *widget = gnc_currency_edit_new ();
    GtkWidget *entry;
    GtkListView *list_view;
    GtkSingleSelection *selection;
    GListModel *model;
    gboolean finalized = FALSE;

    g_object_ref_sink (widget);
    entry = gtk_widget_get_first_child (widget);
    list_view = currency_list_view (widget);
    selection = GTK_SINGLE_SELECTION (gtk_list_view_get_model (list_view));
    model = gtk_single_selection_get_model (selection);
    g_object_weak_ref (G_OBJECT (model), model_finalized, &finalized);

    for (guint index = 0; index < 32; index++)
    {
        gchar *text = g_strdup_printf ("not-a-currency-%u", index);

        gtk_editable_set_text (GTK_EDITABLE (entry), text);
        g_free (text);
    }

    g_object_unref (widget);
    g_assert_true (finalized);
}

int
main (int argc, char **argv)
{
    QofSession *session;
    QofBook *book;
    gnc_commodity_table *table;
    int status;

    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    gnc_engine_init_static (argc, argv);

    session = qof_session_new (qof_book_new ());
    book = qof_session_get_book (session);
    table = gnc_commodity_table_get_table (book);
    gnc_set_current_session (session);
    gnc_commodity_table_insert (
        table, gnc_commodity_new (book, "Alpha currency", GNC_COMMODITY_NS_CURRENCY,
                                  "ALP", "", 100));
    gnc_commodity_table_insert (
        table, gnc_commodity_new (book, "Beta currency", GNC_COMMODITY_NS_CURRENCY,
                                  "BET", "", 100));

    g_test_add_func ("/gnome-utils/currency-edit/model-before-selection",
                     test_model_is_complete_before_selection);
    g_test_add_func ("/gnome-utils/currency-edit/changed-may-destroy-widget",
                     test_changed_handler_may_destroy_widget);
    g_test_add_func ("/gnome-utils/currency-edit/keyboard-popup-no-commit",
                     test_keyboard_popup_does_not_commit);
    g_test_add_func ("/gnome-utils/currency-edit/partial-text-model-ownership",
                     test_partial_text_does_not_retain_model);
    status = g_test_run ();

    gnc_clear_current_session ();
    gnc_engine_shutdown ();
    return status;
}
