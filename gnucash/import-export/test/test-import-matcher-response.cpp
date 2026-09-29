/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "import-main-matcher.h"

static gboolean display_available;

struct Result
{
    guint calls{};
    gboolean accepted{};
};

static void
matcher_finished (gboolean accepted, gpointer user_data)
{
    auto result = static_cast<Result *> (user_data);
    ++result->calls;
    result->accepted = accepted;
}

static GtkWidget *
find_named (GtkWidget *widget, const gchar *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_named (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static void
test_matcher_lifetime (gconstpointer test_data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto action = GPOINTER_TO_INT (test_data);
    auto session = qof_session_new (qof_book_new ());
    gnc_set_current_session (session);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref (parent);
    gtk_widget_show (GTK_WIDGET (parent));
    auto matcher = gnc_gen_trans_list_new (GTK_WIDGET (parent), nullptr,
                                          TRUE, 14, TRUE);
    auto dialog = gnc_gen_trans_list_widget (matcher);
    auto cancel = find_named (dialog, "matcher_cancel");
    g_assert_nonnull (cancel);
    g_object_ref (dialog);
    g_object_ref (cancel);

    Result result;
    gnc_gen_trans_list_present (matcher, matcher_finished, &result);
    g_assert_cmpuint (result.calls, ==, 0);

    switch (action)
    {
    case 0: /* Cancel through the real button signal. */
        gtk_button_clicked (GTK_BUTTON (cancel));
        break;
    case 1: /* The window is destroyed directly. */
        gtk_widget_destroy (dialog);
        break;
    case 2: /* GTK destroys the transient matcher with its owner. */
        gtk_widget_destroy (GTK_WIDGET (parent));
        break;
    case 3: /* The session/component owner explicitly closes it. */
        gnc_gen_trans_list_delete (matcher);
        break;
    default:
        g_assert_not_reached ();
    }

    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_false (result.accepted);

    /* The references intentionally outlive matcher cleanup. Neither a late
     * response nor a retained button signal may call back into freed state. */
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    gtk_button_clicked (GTK_BUTTON (cancel));
    g_assert_cmpuint (result.calls, ==, 1);

    g_object_unref (cancel);
    g_object_unref (dialog);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (parent);
    gnc_clear_current_session ();
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

    const char *names[] = {"cancel", "dialog-destroy", "parent-destroy",
                           "explicit-delete"};
    for (guint i = 0; i < G_N_ELEMENTS (names); ++i)
    {
        auto path = g_strdup_printf ("/import-export/matcher/%s", names[i]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_matcher_lifetime);
        g_free (path);
    }

    auto status = g_test_run ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
