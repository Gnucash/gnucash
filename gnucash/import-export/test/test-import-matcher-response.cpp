/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include <cstdint>

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "import-main-matcher.h"

struct Result
{
    std::uint32_t calls{};
    bool accepted{};
};

static GtkWidget *find_named (GtkWidget *widget, const gchar *name);

class ImportMatcherResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        session = qof_session_new (qof_book_new ());
        gnc_set_current_session (session);
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (parent);
        gtk_widget_show (parent);
        matcher = gnc_gen_trans_list_new (parent, nullptr, true, 14, true);
        ASSERT_NE (matcher, nullptr);
        auto widget = gnc_gen_trans_list_widget (matcher);
        ASSERT_TRUE (GTK_IS_DIALOG (widget));
        dialog = GTK_WIDGET (g_object_ref (widget));
        auto button = find_named (dialog, "matcher_cancel");
        ASSERT_TRUE (GTK_IS_BUTTON (button));
        cancel = GTK_WIDGET (g_object_ref (button));
    }

    void TearDown () override
    {
        if (matcher)
            gnc_gen_trans_list_delete (matcher);
        matcher = nullptr;
        if (parent)
        {
            gtk_widget_destroy (parent);
            g_clear_object (&parent);
        }
        g_clear_object (&cancel);
        g_clear_object (&dialog);
        gnc_clear_current_session ();
    }

    static void matcher_finished (gboolean accepted, gpointer data)
    {
        auto fixture = static_cast<ImportMatcherResponseTest *> (data);
        ++fixture->result.calls;
        fixture->result.accepted = accepted;
        fixture->matcher = nullptr;
    }

    QofSession *session{};
    GtkWidget *parent{};
    GNCImportMainMatcher *matcher{};
    GtkWidget *dialog{};
    GtkWidget *cancel{};
    Result result{};
};

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

TEST_F (ImportMatcherResponseTest, CancelButtonCompletesOnce)
{
    gnc_gen_trans_list_present (matcher, matcher_finished, this);
    EXPECT_EQ (result.calls, 0u);
    gtk_button_clicked (GTK_BUTTON (cancel));
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    gtk_button_clicked (GTK_BUTTON (cancel));
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (ImportMatcherResponseTest, DialogDestroyCompletesOnce)
{
    gnc_gen_trans_list_present (matcher, matcher_finished, this);
    EXPECT_EQ (result.calls, 0u);
    gtk_widget_destroy (dialog);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    gtk_button_clicked (GTK_BUTTON (cancel));
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (ImportMatcherResponseTest, ParentDestroyCompletesOnce)
{
    gnc_gen_trans_list_present (matcher, matcher_finished, this);
    EXPECT_EQ (result.calls, 0u);
    gtk_widget_destroy (parent);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    gtk_button_clicked (GTK_BUTTON (cancel));
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (ImportMatcherResponseTest, OwnerDeleteCompletesOnce)
{
    gnc_gen_trans_list_present (matcher, matcher_finished, this);
    EXPECT_EQ (result.calls, 0u);
    gnc_gen_trans_list_delete (matcher);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    gtk_button_clicked (GTK_BUTTON (cancel));
    EXPECT_EQ (result.calls, 1u);
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("GTK display is required for import matcher response tests");
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects for import matcher tests");
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();

    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto status = RUN_ALL_TESTS ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
