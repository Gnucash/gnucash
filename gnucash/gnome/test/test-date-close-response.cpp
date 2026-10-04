/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include "test-logging.hpp"
#include <cstdint>
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"

extern "C"
{
#include "dialog-date-close.h"
#include "gnc-date-edit.h"
}

namespace
{
struct Result
{
    std::uint32_t calls{0};
    bool accepted{false};
    time64 date{0};
};

static GtkWidget *find_date_dialog ();
static void completed (gboolean accepted, time64 date, gpointer data);

class DateCloseResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (parent);
    }

    void TearDown () override
    {
        gtk_widget_destroy (parent);
        g_object_unref (parent);
        parent = nullptr;
        GnomeResponseTest::TearDown ();
    }

    GtkWidget *parent{};
    Result result;

    GtkWidget *open_dialog ()
    {
        gnc_dialog_date_close_async_parented (
            parent, "Close?", "Date", true, 1234, completed, &result);
        return find_date_dialog ();
    }
};

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
find_date_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node && !result; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-date-close") == 0)
            result = widget;
    }
    g_list_free (windows);
    return result;
}

static GNCDateEdit *
find_date_edit (GtkWidget *root)
{
    if (GNC_IS_DATE_EDIT (root))
        return GNC_DATE_EDIT (root);
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GNCDateEdit *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_date_edit (GTK_WIDGET (node->data));
    g_list_free (children);
    return result;
}

static void
completed (gboolean accepted, time64 date, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->accepted = accepted;
    result->date = date;
}

static void
destroy_parent_during_completion (GtkWidget *, gpointer parent)
{
    gtk_widget_destroy (GTK_WIDGET(parent));
}

TEST_F (DateCloseResponseTest, CancelCompletesOnceAsRejected)
{
    auto dialog = open_dialog ();
    ASSERT_NE (dialog, nullptr);
    gtk_button_clicked (GTK_BUTTON (find_buildable (dialog, "cancelbutton")));
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
}

TEST_F (DateCloseResponseTest, OkReturnsSelectedDate)
{
    auto dialog = open_dialog ();
    ASSERT_NE (dialog, nullptr);
    auto date_edit = find_date_edit (dialog);
    ASSERT_NE (date_edit, nullptr);
    auto expected_date = gnc_date_edit_get_date (date_edit);
    gtk_button_clicked (GTK_BUTTON (find_buildable (dialog, "okbutton")));
    EXPECT_TRUE (result.accepted);
    EXPECT_EQ (result.date, expected_date);
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (DateCloseResponseTest, ParentDestroyCancelsAndLateResponseIsIgnored)
{
    auto dialog = open_dialog ();
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    gtk_widget_destroy (parent);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    g_object_unref (dialog);
}

TEST_F (DateCloseResponseTest, ParentDestroyDuringAcceptanceCancels)
{
    auto dialog = open_dialog ();
    ASSERT_NE (dialog, nullptr);
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (destroy_parent_during_completion), parent);
    gtk_button_clicked (GTK_BUTTON (find_buildable (dialog, "okbutton")));
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
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
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
