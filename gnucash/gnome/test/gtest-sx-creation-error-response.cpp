/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include "googletest-glib-log-handler.hpp"
#include <cstdint>
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"
#include "dialog-sx-since-last-run.h"


class ScheduledTransactionErrorResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
    }

    void TearDown () override
    {
        if (dialog)
        {
            gtk_widget_destroy (dialog);
            while (g_main_context_iteration (nullptr, FALSE))
                ;
        }
        GnomeResponseTest::TearDown ();
    }

    GtkWidget *dialog{};
};

TEST_F (ScheduledTransactionErrorResponseTest, ErrorListIsConsumedBeforeResponse)
{
    GList *errors = g_list_append (nullptr, g_strdup ("Synthetic creation error"));
    gnc_ui_sx_creation_error_dialog (&errors);
    ASSERT_EQ (errors, nullptr);
    auto windows = gtk_window_list_toplevels ();
    std::uint32_t message_dialog_count = 0;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data))
        {
            ++message_dialog_count;
            dialog = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    ASSERT_EQ (message_dialog_count, 1u);
    ASSERT_NE (dialog, nullptr);
    gchar *message = nullptr;
    g_object_get (dialog, "secondary-text", &message, nullptr);
    EXPECT_STREQ (message, "Synthetic creation error");
    g_free (message);
    gpointer weak_dialog = dialog;
    g_object_add_weak_pointer (G_OBJECT (dialog), &weak_dialog);
    /* Calling again with the consumed list remains harmless. */
    gnc_ui_sx_creation_error_dialog (&errors);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
    EXPECT_EQ (weak_dialog, nullptr);
    dialog = nullptr;
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
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
