/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef GNC_GNOME_RESPONSE_TEST_FIXTURE_H
#define GNC_GNOME_RESPONSE_TEST_FIXTURE_H

#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "gnc-main-window.h"
#include <algorithm>
#include <vector>

static bool
is_application_toplevel (GtkWidget *widget)
{
    /* File chooser buttons own hidden dialogs; their owner disposes them. */
    return GNC_IS_MAIN_WINDOW (widget) ||
           (GTK_IS_DIALOG (widget) && !GTK_IS_FILE_CHOOSER (widget)) ||
           GTK_IS_ASSISTANT (widget);
}

class GnomeResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        ASSERT_NE (gdk_display_get_default (), nullptr);
        auto windows = gtk_window_list_toplevels ();
        for (auto node = windows; node; node = node->next)
        {
            auto window = GTK_WIDGET (node->data);
            if (is_application_toplevel (window))
                windows_before_test.push_back (GTK_WIDGET (g_object_ref (window)));
        }
        g_list_free (windows);
    }

    void TearDown () override
    {
        std::vector<GtkWidget *> windows_to_destroy;
        auto windows = gtk_window_list_toplevels ();
        for (auto node = windows; node; node = node->next)
        {
            auto window = GTK_WIDGET (node->data);
            if (is_application_toplevel (window) &&
                std::find (windows_before_test.begin (), windows_before_test.end (),
                           window) == windows_before_test.end ())
                windows_to_destroy.push_back (GTK_WIDGET (g_object_ref (window)));
        }
        g_list_free (windows);
        for (auto window : windows_to_destroy)
        {
            gtk_widget_destroy (window);
            g_object_unref (window);
        }
        while (g_main_context_iteration (nullptr, false))
            ;
        for (auto window : windows_before_test)
        {
            g_object_unref (window);
        }
    }

private:
    std::vector<GtkWidget *> windows_before_test;
};

#endif /* GNC_GNOME_RESPONSE_TEST_FIXTURE_H */
