/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
#include "dialog-utils.h"
#include "gnc-gsettings.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"

static gboolean display_available;
static constexpr const char *remembered_response_key = "inv-entry-dup";

struct Completion
{
    guint count = 0;
    gint response = GTK_RESPONSE_NONE;
    GtkWindow *parent = nullptr;
};

static void
completed (GtkWindow *parent, gint response, gpointer user_data)
{
    auto result = static_cast<Completion *> (user_data);
    ++result->count;
    result->response = response;
    result->parent = parent;
}

static GtkWidget *
find_query (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            g_assert_null (dialog);
            dialog = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return dialog;
}

static void
destroy_parent_on_query_destroy (GtkWidget *, gpointer parent)
{
    gtk_widget_destroy (GTK_WIDGET (parent));
}

static void
test_query (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    auto family = scenario / 7;
    auto action = scenario % 7;
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    Completion result;
    gint accept = GTK_RESPONSE_OK;
    gint cancel = GTK_RESPONSE_CANCEL;
    if (family == 0)
        gnc_ok_cancel_dialog_async (parent, GTK_RESPONSE_CANCEL, completed,
                                    &result, "query %d", scenario);
    else if (family == 1)
    {
        accept = GTK_RESPONSE_YES;
        cancel = GTK_RESPONSE_NO;
        gnc_verify_dialog_async (parent, FALSE, completed, &result,
                                 "query %d", scenario);
    }
    else
    {
        accept = GTK_RESPONSE_ACCEPT;
        gnc_action_dialog_async (parent, "Action", FALSE, completed,
                                 &result, "query %d", scenario);
    }
    g_assert_cmpuint (result.count, ==, 0);
    auto dialog = find_query (parent);
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));

    /* Retain only the object, not its callback state: a late response after
     * completion must not call the consumer or touch the freed request. */
    g_object_ref (dialog);
    switch (action)
    {
    case 0: gtk_dialog_response (GTK_DIALOG (dialog), accept); break;
    case 1: gtk_dialog_response (GTK_DIALOG (dialog), cancel); break;
    case 2: gtk_window_close (GTK_WINDOW (dialog)); break;
    case 3: gtk_widget_destroy (GTK_WIDGET (parent)); break;
    case 4: gtk_widget_destroy (dialog); break;
    case 5: gtk_dialog_response (GTK_DIALOG (dialog), 12345); break;
    case 6:
        g_object_ref (parent);
        g_signal_connect (dialog, "destroy",
                          G_CALLBACK (destroy_parent_on_query_destroy), parent);
        gtk_dialog_response (GTK_DIALOG (dialog), accept);
        break;
    }
    for (guint attempt = 0; result.count == 0 && attempt < 1000; ++attempt)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (result.count == 0)
            g_usleep (1000);
    }
    g_assert_cmpuint (result.count, ==, 1);
    g_assert_cmpint (result.response, ==, action == 0 ? accept : cancel);
    if (action == 3 || action == 6)
        g_assert_null (result.parent);
    else
        g_assert_true (result.parent == parent);
    gtk_dialog_response (GTK_DIALOG (dialog), accept);
    g_assert_cmpuint (result.count, ==, 1);
    g_object_unref (dialog);
    if (action == 6)
        g_object_unref (parent);
    else if (action != 3)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

static void
notice_destroyed (GtkWidget *, gpointer user_data)
{
    ++*static_cast<guint *> (user_data);
}

static void
test_notice (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto family = GPOINTER_TO_INT (data) / 4;
    auto action = GPOINTER_TO_INT (data) % 4;
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    auto detail = g_strdup ("borrowed detail");
    if (family == 0)
        gnc_error_dialog_async (parent, "Notice %d: %s", 42, detail);
    else if (family == 1)
        gnc_warning_dialog_async (parent, "Notice %d: %s", 42, detail);
    else
        gnc_info_dialog_async (parent, "Notice %d: %s", 42, detail);
    g_free (detail);
    auto dialog = find_query (parent);
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    gchar *message = nullptr;
    gint type = GTK_MESSAGE_OTHER;
    g_object_get (dialog, "text", &message, "message-type", &type, nullptr);
    const GtkMessageType types[] = {GTK_MESSAGE_ERROR, GTK_MESSAGE_WARNING,
                                    GTK_MESSAGE_INFO};
    g_assert_cmpstr (message, ==, "Notice 42: borrowed detail");
    g_assert_cmpint (type, ==, types[family]);
    g_free (message);

    guint count = 0;
    g_signal_connect (dialog, "destroy", G_CALLBACK (notice_destroyed), &count);
    g_object_ref (dialog);
    GtkWidget *weak_dialog = dialog;
    g_object_add_weak_pointer (G_OBJECT (dialog),
                               reinterpret_cast<gpointer *> (&weak_dialog));
    switch (action)
    {
    case 0: gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE); break;
    case 1: gtk_window_close (GTK_WINDOW (dialog)); break;
    case 2: gtk_widget_destroy (GTK_WIDGET (parent)); break;
    case 3: gtk_widget_destroy (dialog); break;
    }
    for (guint attempt = 0; count == 0 && attempt < 1000; ++attempt)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (count == 0)
            g_usleep (1000);
    }
    g_assert_cmpuint (count, ==, 1);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
    g_assert_cmpuint (count, ==, 1);
    g_object_unref (dialog);
    g_assert_null (weak_dialog);
    if (action != 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

static void
test_error_list (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    GList *errors = nullptr;
    if (scenario == 4)
    {
        errors = g_list_append (errors, const_cast<char *> ("First: 100%"));
        errors = g_list_append (errors, const_cast<char *> ("Second: äöü"));
    }
    else if (scenario != 0)
    {
        errors = g_list_append (errors, g_strdup ("First: 100%"));
        if (scenario != 1)
            errors = g_list_append (errors, g_strdup ("Second: äöü"));
    }
    gnc_error_dialog_async_list (parent, errors);
    if (scenario == 4)
        g_list_free (errors);
    else
        g_list_free_full (errors, g_free);
    auto dialog = find_query (parent);
    if (scenario == 0)
        g_assert_null (dialog);
    else
    {
        g_assert_nonnull (dialog);
        gchar *text = nullptr;
        g_object_get (dialog, "text", &text, nullptr);
        g_assert_cmpstr (text, ==, scenario == 1 ? "First: 100%" :
                         "First: 100%\n\nSecond: äöü");
        g_free (text);
        g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
        guint destroyed = 0;
        g_signal_connect (dialog, "destroy", G_CALLBACK (notice_destroyed),
                          &destroyed);
        g_object_ref (dialog);
        if (scenario == 3)
            gtk_widget_destroy (GTK_WIDGET (parent));
        else
            gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
        g_assert_cmpuint (destroyed, ==, 1);
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
        g_assert_cmpuint (destroyed, ==, 1);
        g_object_unref (dialog);
    }
    if (scenario != 3)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

struct Info2Completion
{
    guint calls{0};
    GtkWindow *parent{nullptr};
    gint response{GTK_RESPONSE_NONE};
};

static void
info2_completed (GtkWindow *parent, gint response, gpointer user_data)
{
    auto result = static_cast<Info2Completion *> (user_data);
    ++result->calls;
    result->parent = parent;
    result->response = response;
}

static GtkWidget *
find_info2_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) == parent &&
            g_strcmp0 (gtk_window_get_title (GTK_WINDOW (widget)),
                       "Copied title") == 0)
        {
            g_assert_null (dialog);
            dialog = widget;
        }
    }
    g_list_free (windows);
    return dialog;
}

static void
test_info2_async (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    auto title = g_strdup ("Copied title");
    auto text = g_strdup ("Copied message");
    Info2Completion result;
    gnc_info2_dialog_async (GTK_WIDGET (parent), title, text,
                            info2_completed, &result);
    g_free (title);
    g_free (text);

    auto dialog = find_info2_dialog (parent);
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto children = gtk_container_get_children (
        GTK_CONTAINER (gtk_dialog_get_content_area (GTK_DIALOG (dialog))));
    g_assert_nonnull (children);
    auto view = gtk_bin_get_child (GTK_BIN (children->data));
    g_list_free (children);
    g_assert_true (GTK_IS_TEXT_VIEW (view));
    GtkTextIter start, end;
    auto buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
    gtk_text_buffer_get_bounds (buffer, &start, &end);
    auto copied_text = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
    g_assert_cmpstr (copied_text, ==, "Copied message");
    g_free (copied_text);

    g_object_ref (dialog);
    if (scenario == 0)
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    else if (scenario == 1)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else
        gtk_widget_destroy (dialog);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_true (result.parent == (scenario == 1 ? nullptr : parent));
    g_assert_cmpint (result.response, ==,
                     scenario == 0 ? GTK_RESPONSE_ACCEPT : GTK_RESPONSE_CANCEL);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (dialog);
    if (scenario != 1)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

static void
test_info_dialog_async_response (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    auto text = g_strdup ("Copied summary");
    Info2Completion result;
    gnc_info_dialog_async_response (parent, info2_completed, &result,
                                    "%s", text);
    g_free (text);

    auto dialog = find_query (parent);
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    gchar *message = nullptr;
    gint type = GTK_MESSAGE_OTHER;
    g_object_get (dialog, "text", &message, "message-type", &type, nullptr);
    g_assert_cmpstr (message, ==, "Copied summary");
    g_assert_cmpint (type, ==, GTK_MESSAGE_INFO);
    g_free (message);

    g_object_ref (dialog);
    if (scenario == 0)
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
    else
        gtk_widget_destroy (GTK_WIDGET (parent));
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_true (result.parent == (scenario == 0 ? parent : nullptr));
    g_assert_cmpint (result.response, ==,
                     scenario == 0 ? GTK_RESPONSE_CLOSE : GTK_RESPONSE_CANCEL);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (dialog);
    if (scenario == 0)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

struct DialogRunResult
{
    guint count = 0;
    gint response = GTK_RESPONSE_NONE;
    GtkWindow *parent = nullptr;
};

static void
dialog_run_completed (GtkWindow *parent, gint response, gpointer user_data)
{
    auto result = static_cast<DialogRunResult *> (user_data);
    ++result->count;
    result->response = response;
    result->parent = parent;
}

static GtkWidget *
find_check_button (GtkWidget *widget, const gchar *label)
{
    if (GTK_IS_CHECK_BUTTON (widget) &&
        g_strcmp0 (gtk_button_get_label (GTK_BUTTON (widget)), label) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_check_button (GTK_WIDGET (node->data), label);
    g_list_free (children);
    return found;
}

static void
destroy_parent_on_pref_change (gpointer prefs G_GNUC_UNUSED,
                               gchar *pref G_GNUC_UNUSED, gpointer user_data)
{
    gtk_widget_destroy (GTK_WIDGET (user_data));
}

static void
destroy_dialog_on_modal_notify (GtkWidget *dialog, GParamSpec *pspec G_GNUC_UNUSED,
                                gpointer user_data G_GNUC_UNUSED)
{
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    gtk_widget_destroy (dialog);
}

static void
test_dialog_run_async (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto scenario = GPOINTER_TO_INT (data);
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_PERM,
                       remembered_response_key, 0);
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                       remembered_response_key, 0);
    if (scenario == 3)
        gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                           remembered_response_key, GTK_RESPONSE_REJECT);

    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    auto dialog = GTK_DIALOG (gtk_message_dialog_new (
        parent, static_cast<GtkDialogFlags> (
            GTK_DIALOG_DESTROY_WITH_PARENT | (scenario >= 5 ? 0 : GTK_DIALOG_MODAL)),
        GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "%s", "Record this entry?"));
    gtk_dialog_add_button (dialog, "Cancel", GTK_RESPONSE_CANCEL);
    gtk_dialog_add_button (dialog, "Don't Record", GTK_RESPONSE_REJECT);
    gtk_dialog_add_button (dialog, "Record", GTK_RESPONSE_ACCEPT);
    DialogRunResult result;
    if (scenario == 3)
        g_object_ref (dialog);
    if (scenario == 4)
        gnc_prefs_register_cb (GNC_PREFS_GROUP_WARNINGS_PERM,
                               remembered_response_key,
                               (gpointer)destroy_parent_on_pref_change, parent);
    if (scenario == 6)
        g_signal_connect (dialog, "notify::modal",
                          G_CALLBACK (destroy_dialog_on_modal_notify), nullptr);
    if (scenario == 6)
        g_object_ref (dialog);

    gnc_dialog_run_async (dialog, scenario == 5 ? nullptr : remembered_response_key,
                          dialog_run_completed, &result);
    if (scenario == 3)
    {
        g_assert_cmpuint (result.count, ==, 1);
        g_assert_true (result.parent == parent);
        g_assert_cmpint (result.response, ==, GTK_RESPONSE_REJECT);
        gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
        g_assert_cmpuint (result.count, ==, 1);
        g_object_unref (dialog);
        gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                           remembered_response_key, 0);
        gtk_widget_destroy (GTK_WIDGET (parent));
        return;
    }
    if (scenario == 6)
    {
        g_assert_cmpuint (result.count, ==, 1);
        g_assert_true (result.parent == parent);
        g_assert_cmpint (result.response, ==, GTK_RESPONSE_CANCEL);
        gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
        g_assert_cmpuint (result.count, ==, 1);
        g_object_unref (dialog);
        gtk_widget_destroy (GTK_WIDGET (parent));
        return;
    }

    g_assert_cmpuint (result.count, ==, 0);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_object_ref (dialog);
    if (scenario == 5)
    {
        gtk_dialog_response (dialog, GTK_RESPONSE_CLOSE);
        g_assert_cmpuint (result.count, ==, 1);
        g_assert_true (result.parent == parent);
        g_assert_cmpint (result.response, ==, GTK_RESPONSE_CLOSE);
        gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
        g_assert_cmpuint (result.count, ==, 1);
        g_object_unref (dialog);
        gtk_widget_destroy (GTK_WIDGET (parent));
        return;
    }
    auto permanent = find_check_button (GTK_WIDGET (dialog),
                                        "Remember and don't _ask me again.");
    auto temporary = find_check_button (GTK_WIDGET (dialog),
                                        "Remember and don't ask me again this _session.");
    g_assert_nonnull (permanent);
    g_assert_nonnull (temporary);
    if (scenario == 0 || scenario == 2 || scenario == 4)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (permanent), TRUE);
    else if (scenario == 1)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (temporary), TRUE);
    gint selected_response = scenario == 1 ? GTK_RESPONSE_REJECT :
                             scenario == 2 ? GTK_RESPONSE_CANCEL :
                             GTK_RESPONSE_ACCEPT;
    gtk_dialog_response (dialog, selected_response);
    g_assert_cmpuint (result.count, ==, 1);
    if (scenario == 4)
    {
        g_assert_null (result.parent);
        g_assert_cmpint (result.response, ==, GTK_RESPONSE_CANCEL);
        gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_WARNINGS_PERM,
                                     remembered_response_key,
                                     (gpointer)destroy_parent_on_pref_change,
                                     parent);
    }
    else
    {
        g_assert_true (result.parent == parent);
        g_assert_cmpint (result.response, ==, selected_response);
    }
    gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
    g_assert_cmpuint (result.count, ==, 1);
    g_object_unref (dialog);

    gint permanent_value = gnc_prefs_get_int (GNC_PREFS_GROUP_WARNINGS_PERM,
                                               remembered_response_key);
    gint temporary_value = gnc_prefs_get_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                                               remembered_response_key);
    /* The preference is committed before its notification destroys the parent.
     * Only the caller's action is cancelled; do not roll back a notified value. */
    if (scenario == 0 || scenario == 4)
        g_assert_cmpint (permanent_value, ==, GTK_RESPONSE_ACCEPT);
    else if (scenario == 1)
        g_assert_cmpint (temporary_value, ==, GTK_RESPONSE_REJECT);
    else
    {
        g_assert_cmpint (permanent_value, ==, 0);
        g_assert_cmpint (temporary_value, ==, 0);
    }
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_PERM,
                       remembered_response_key, 0);
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                       remembered_response_key, 0);
    if (scenario != 4)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

static GtkWidget *
find_child_type (GtkWidget *widget, GType type)
{
    if (g_type_is_a (G_OBJECT_TYPE (widget), type))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_child_type (GTK_WIDGET (node->data), type);
    g_list_free (children);
    return found;
}

static GtkDialog *
find_plain_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            g_assert_null (found);
            found = GTK_DIALOG (node->data);
        }
    g_list_free (windows);
    return found;
}

static void
test_input_query (gconstpointer data)
{
    if (!display_available) { g_test_skip ("No graphical display is available"); return; }
    auto scenario = GPOINTER_TO_INT (data);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref (parent);
    struct InputResult { unsigned count = 0; gchar *text = nullptr; } result;
    auto callback = +[](GtkWindow *, gchar *text, gpointer data) {
        auto result = static_cast<InputResult *>(data);
        ++result->count;
        result->text = text;
    };
    if (scenario < 3)
        gnc_input_dialog_with_entry_async (GTK_WIDGET (parent), "Input", "Value", "initial", callback, &result);
    else
        gnc_input_dialog_async (GTK_WIDGET (parent), "Input", "Value", "initial", callback, &result);
    g_assert_cmpuint (result.count, ==, 0);
    auto dialog = find_plain_dialog (parent);
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    auto view = find_child_type (GTK_WIDGET (dialog), scenario < 3 ? GTK_TYPE_ENTRY : GTK_TYPE_TEXT_VIEW);
    g_assert_nonnull (view);
    if (scenario < 3)
        gtk_entry_set_text (GTK_ENTRY (view), "response value");
    else
        gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)), "response value", -1);
    if (scenario % 3 == 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else
        gtk_dialog_response (dialog, scenario % 3 == 0 ? GTK_RESPONSE_ACCEPT : GTK_RESPONSE_REJECT);
    g_assert_cmpuint (result.count, ==, 1);
    if (scenario % 3 == 0)
        g_assert_cmpstr (result.text, ==, "response value");
    else
        g_assert_null (result.text);
    gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
    g_assert_cmpuint (result.count, ==, 1);
    g_free (result.text);
    g_object_unref (dialog);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (parent);
}

static void
test_radio_query (gconstpointer data)
{
    if (!display_available) { g_test_skip ("No graphical display is available"); return; }
    auto scenario = GPOINTER_TO_INT (data);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref (parent);
    auto labels = g_list_append (nullptr, const_cast<char *> ("First"));
    labels = g_list_append (labels, const_cast<char *> ("Second"));
    Completion result;
    gnc_choose_radio_option_dialog_async (GTK_WIDGET (parent), "Choice", "Choose", nullptr,
                                          1, labels, completed, &result);
    g_list_free (labels);
    g_assert_cmpuint (result.count, ==, 0);
    auto dialog = find_plain_dialog (parent);
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    auto radio = find_child_type (GTK_WIDGET (dialog), GTK_TYPE_RADIO_BUTTON);
    g_assert_nonnull (radio);
    g_object_ref (radio);
    if (scenario == 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else
        gtk_dialog_response (dialog, scenario == 0 ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);
    g_assert_cmpuint (result.count, ==, 1);
    g_assert_cmpint (result.response, ==, scenario == 0 ? 1 : -1);
    /* Retained child objects must no longer point at the freed selection. */
    gtk_button_clicked (GTK_BUTTON (radio));
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    g_assert_cmpuint (result.count, ==, 1);
    g_object_unref (radio);
    g_object_unref (dialog);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (parent);
}

int
main (int argc, char **argv)
{
    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    const gchar *builddir = g_getenv ("GNC_BUILDDIR");
    if (builddir)
    {
        auto schema_dir = g_build_filename (builddir, "share",
                                            "glib-2.0", "schemas", nullptr);
        g_setenv ("GSETTINGS_SCHEMA_DIR", schema_dir, TRUE);
        g_free (schema_dir);
    }
    gnc_gsettings_load_backend ();
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    const char *families[] = {"ok-cancel", "verify", "action"};
    const char *actions[] = {"accept", "cancel", "window-close", "parent-destroy",
                             "dialog-destroy", "invalid-response",
                             "parent-destroy-during-completion"};
    for (guint family = 0; family < G_N_ELEMENTS (families); ++family)
        for (guint action = 0; action < G_N_ELEMENTS (actions); ++action)
        {
            auto path = g_strdup_printf ("/gnome-utils/query/%s/%s",
                                         families[family], actions[action]);
            g_test_add_data_func (path, GINT_TO_POINTER (family * 7 + action), test_query);
            g_free (path);
        }
    const char *notice_actions[] = {"close", "window-close", "parent-destroy",
                                    "dialog-destroy"};
    const char *notice_families[] = {"error-notice", "warning-notice", "info-notice"};
    for (guint family = 0; family < G_N_ELEMENTS (notice_families); ++family)
        for (guint action = 0; action < G_N_ELEMENTS (notice_actions); ++action)
        {
            auto path = g_strdup_printf ("/gnome-utils/query/%s/%s",
                                         notice_families[family], notice_actions[action]);
            g_test_add_data_func (path, GINT_TO_POINTER (family * 4 + action), test_notice);
            g_free (path);
        }
    const char *list_cases[] = {"empty", "single", "ordered-copy", "parent-destroy",
                               "borrowed-text"};
    for (guint scenario = 0; scenario < G_N_ELEMENTS (list_cases); ++scenario)
    {
        auto path = g_strdup_printf ("/gnome-utils/query/error-list/%s",
                                     list_cases[scenario]);
        g_test_add_data_func (path, GINT_TO_POINTER (scenario), test_error_list);
        g_free (path);
    }
    const char *info2_cases[] = {"accept", "parent-destroy", "dialog-destroy"};
    for (guint scenario = 0; scenario < G_N_ELEMENTS (info2_cases); ++scenario)
    {
        auto path = g_strdup_printf ("/gnome-utils/query/info2/%s",
                                     info2_cases[scenario]);
        g_test_add_data_func (path, GINT_TO_POINTER (scenario), test_info2_async);
        g_free (path);
    }
    const char *info_cases[] = {"dismiss", "parent-destroy"};
    for (guint scenario = 0; scenario < G_N_ELEMENTS (info_cases); ++scenario)
    {
        auto path = g_strdup_printf ("/gnome-utils/query/info-response/%s",
                                     info_cases[scenario]);
        g_test_add_data_func (path, GINT_TO_POINTER (scenario),
                              test_info_dialog_async_response);
        g_free (path);
    }
    const char *run_cases[] = {"record-permanent", "dont-record-session",
                               "cancel-not-remembered", "cached-response",
                               "preference-destroys-parent", "pref-null-modal",
                               "modal-notification-destroys-dialog"};
    for (guint scenario = 0; scenario < G_N_ELEMENTS (run_cases); ++scenario)
    {
        auto path = g_strdup_printf ("/gnome-utils/query/dialog-run/%s",
                                     run_cases[scenario]);
        g_test_add_data_func (path, GINT_TO_POINTER (scenario),
                              test_dialog_run_async);
        g_free (path);
    }
    const char *input_cases[] = {"entry-accept", "entry-cancel", "entry-owner-destroy",
                                 "text-accept", "text-cancel", "text-owner-destroy"};
    for (guint i = 0; i < G_N_ELEMENTS (input_cases); ++i)
    {
        auto path = g_strdup_printf ("/gnome-utils/query/input/%s", input_cases[i]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_input_query);
        g_free (path);
    }
    const char *radio_cases[] = {"accept-default", "cancel", "owner-destroy"};
    for (guint i = 0; i < G_N_ELEMENTS (radio_cases); ++i)
    {
        auto path = g_strdup_printf ("/gnome-utils/query/radio/%s", radio_cases[i]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_radio_query);
        g_free (path);
    }
    auto status = g_test_run ();
    gnc_gsettings_shutdown ();
    return status;
}
