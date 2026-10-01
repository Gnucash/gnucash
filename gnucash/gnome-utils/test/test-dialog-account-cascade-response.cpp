/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <cstdint>

#include <gtk/gtk.h>

#include <gtest/gtest.h>

#include <algorithm>
#include <vector>

#include "Account.h"
#include "cashobjects.h"
#include "dialog-account.h"
#include "qof.h"
#include "qofbook.h"
#include "qofevent.h"

struct RemoveChildOnModify
{
    Account *target{};
    Account *child{};
    bool removed{};
};

static GtkWidget *find_cascade_dialog ();

static void
remove_child_on_modify (QofInstance *entity, QofEventId event,
                        gpointer user_data, [[maybe_unused]] gpointer event_data)
{
    auto removal = static_cast<RemoveChildOnModify *> (user_data);
    if (!removal->removed && event == QOF_EVENT_MODIFY &&
        entity == QOF_INSTANCE (removal->target))
    {
        removal->removed = true;
        xaccAccountBeginEdit (removal->child);
        xaccAccountDestroy (removal->child);
        removal->child = nullptr;
    }
}

class AccountCascadeResponseTest : public ::testing::Test
{
protected:
    static void SetUpTestSuite ()
    {
        qof_init ();
        ASSERT_TRUE (cashobjects_register ());
    }

    static void TearDownTestSuite ()
    {
        qof_close ();
    }

    void SetUp () override
    {
        m_book = qof_book_new ();
        m_book_root = gnc_account_create_root (m_book);
        m_account = xaccMallocAccount (m_book);
        m_child = xaccMallocAccount (m_book);
        m_grandchild = xaccMallocAccount (m_book);
        xaccAccountSetName (m_account, "Cascade target");
        xaccAccountSetName (m_child, "Cascade child");
        xaccAccountSetName (m_grandchild, "Cascade grandchild");
        gnc_account_append_child (m_book_root, m_account);
        gnc_account_append_child (m_account, m_child);
        gnc_account_append_child (m_child, m_grandchild);

        m_parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (m_parent);
    }

    void TearDown () override
    {
        unregister_event_handler ();
        if (m_weak_dialog)
        {
            g_object_remove_weak_pointer (G_OBJECT (m_weak_dialog),
                                          reinterpret_cast<gpointer *> (
                                              &m_weak_dialog));
            m_weak_dialog = nullptr;
        }
        if (m_parent && !gtk_widget_in_destruction (m_parent))
            gtk_widget_destroy (m_parent);
        for (auto dialog : m_dialogs)
        {
            if (!gtk_widget_in_destruction (dialog))
                gtk_widget_destroy (dialog);
            g_object_unref (dialog);
        }
        m_dialogs.clear ();
        if (m_parent)
        {
            g_object_unref (m_parent);
            m_parent = nullptr;
        }
        if (m_book)
        {
            qof_book_destroy (m_book);
            m_book = nullptr;
        }
        m_book_root = nullptr;
        m_account = nullptr;
        m_child = nullptr;
        m_grandchild = nullptr;
    }

    GtkWidget *start_dialog ()
    {
        gnc_account_cascade_properties_dialog (m_parent, m_account);
        auto dialog = find_cascade_dialog ();
        if (dialog)
            m_dialogs.push_back (GTK_WIDGET (g_object_ref (dialog)));
        return dialog;
    }

    void release_dialog (GtkWidget *dialog)
    {
        auto found = std::find (m_dialogs.begin (), m_dialogs.end (), dialog);
        ASSERT_NE (found, m_dialogs.end ());
        m_dialogs.erase (found);
        g_object_unref (dialog);
    }

    void register_event_handler ()
    {
        m_removal = {m_account, m_child, false};
        m_event_handler = qof_event_register_handler (remove_child_on_modify,
                                                       &m_removal);
    }

    void unregister_event_handler ()
    {
        if (m_event_handler)
        {
            qof_event_unregister_handler (m_event_handler);
            m_event_handler = 0;
        }
    }

    QofBook *m_book{};
    Account *m_book_root{};
    Account *m_account{};
    Account *m_child{};
    Account *m_grandchild{};
    GtkWidget *m_parent{};
    GtkWidget *m_weak_dialog{};
    std::vector<GtkWidget *> m_dialogs;
    RemoveChildOnModify m_removal{};
    std::int32_t m_event_handler{};
};

static GtkWidget *
find_buildable (GtkWidget *widget, const gchar *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;

    if (!GTK_IS_CONTAINER (widget))
        return nullptr;

    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_buildable (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static GtkWidget *
find_cascade_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node && !dialog; node = node->next)
        dialog = find_buildable (GTK_WIDGET (node->data),
                                 "account_cascade_dialog");
    g_list_free (windows);
    return dialog;
}

static GtkWidget *
control (GtkWidget *dialog, const gchar *name)
{
    auto widget = find_buildable (dialog, name);
    EXPECT_NE (widget, nullptr);
    return widget;
}

static void
select_all_updates (GtkWidget *dialog, bool replace)
{
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_color")), true);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "replace_check")), replace);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_placeholder")), true);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "placeholder_check_button")), true);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_hidden")), true);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "hidden_check_button")), true);

    GdkRGBA color{0.2, 0.4, 0.6, 1.0};
    gtk_color_chooser_set_rgba (
        GTK_COLOR_CHOOSER (control (dialog, "color_button")), &color);
}

TEST_F (AccountCascadeResponseTest, ApplyAndLateResponse)
{
    xaccAccountSetColor (m_account, "red");
    xaccAccountSetColor (m_child, "blue");
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    select_all_updates (dialog, true);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    GdkRGBA color{0.2, 0.4, 0.6, 1.0};
    auto expected_color = gdk_rgba_to_string (&color);
    EXPECT_STREQ (xaccAccountGetColor (m_account), expected_color);
    EXPECT_STREQ (xaccAccountGetColor (m_child), expected_color);
    EXPECT_STREQ (xaccAccountGetColor (m_grandchild), expected_color);
    EXPECT_TRUE (xaccAccountGetPlaceholder (m_account));
    EXPECT_TRUE (xaccAccountGetPlaceholder (m_child));
    EXPECT_TRUE (xaccAccountGetHidden (m_account));
    EXPECT_TRUE (xaccAccountGetHidden (m_child));
    EXPECT_TRUE (xaccAccountGetHidden (m_grandchild));

    xaccAccountSetHidden (m_child, false);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_FALSE (xaccAccountGetHidden (m_child));
    g_free (expected_color);
}

TEST_F (AccountCascadeResponseTest, ReplaceDisabled)
{
    xaccAccountSetColor (m_account, "red");
    xaccAccountSetColor (m_child, "blue");
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, false);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_placeholder")), false);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_hidden")), false);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    EXPECT_STREQ (xaccAccountGetColor (m_account), "red");
    EXPECT_STREQ (xaccAccountGetColor (m_child), "blue");
    GdkRGBA color{0.2, 0.4, 0.6, 1.0};
    auto expected_color = gdk_rgba_to_string (&color);
    EXPECT_STREQ (xaccAccountGetColor (m_grandchild), expected_color);
    g_free (expected_color);
}

TEST_F (AccountCascadeResponseTest, CancelDoesNotUpdateAccounts)
{
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, true);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (xaccAccountGetColor (m_account), nullptr);
    EXPECT_FALSE (xaccAccountGetPlaceholder (m_child));
    EXPECT_FALSE (xaccAccountGetHidden (m_child));
}

TEST_F (AccountCascadeResponseTest, ParentDestroyClosesDialogWithoutUpdating)
{
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, true);
    gtk_widget_destroy (m_parent);
    EXPECT_EQ (find_cascade_dialog (), nullptr);
    EXPECT_EQ (xaccAccountGetColor (m_account), nullptr);
    EXPECT_FALSE (xaccAccountGetPlaceholder (m_child));
    EXPECT_FALSE (xaccAccountGetHidden (m_child));
}

TEST_F (AccountCascadeResponseTest, DialogDestroyIgnoresLateResponse)
{
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, true);
    m_weak_dialog = dialog;
    g_object_add_weak_pointer (G_OBJECT (dialog),
                               reinterpret_cast<gpointer *> (&m_weak_dialog));

    gtk_widget_destroy (dialog);
    EXPECT_EQ (xaccAccountGetColor (m_account), nullptr);
    EXPECT_FALSE (xaccAccountGetPlaceholder (m_child));
    EXPECT_FALSE (xaccAccountGetHidden (m_child));
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (xaccAccountGetColor (m_account), nullptr);
    EXPECT_FALSE (xaccAccountGetPlaceholder (m_child));
    EXPECT_FALSE (xaccAccountGetHidden (m_child));
    release_dialog (dialog);
    EXPECT_EQ (m_weak_dialog, nullptr);
}

TEST_F (AccountCascadeResponseTest, ReadonlyBookDoesNotUpdateAccounts)
{
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, true);
    qof_book_mark_readonly (m_book);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (xaccAccountGetColor (m_account), nullptr);
    EXPECT_FALSE (xaccAccountGetHidden (m_child));
}

TEST_F (AccountCascadeResponseTest, ClosedBookDoesNotUpdateAccounts)
{
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, true);
    qof_book_mark_closed (m_book);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (xaccAccountGetColor (m_account), nullptr);
    EXPECT_FALSE (xaccAccountGetPlaceholder (m_child));
}

TEST_F (AccountCascadeResponseTest, RemovedAccountIsIgnored)
{
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, true);
    xaccAccountBeginEdit (m_account);
    xaccAccountDestroy (m_account);
    m_account = nullptr;
    m_child = nullptr;
    m_grandchild = nullptr;
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (find_cascade_dialog (), nullptr);
}

TEST_F (AccountCascadeResponseTest, DestroyedBookIsIgnored)
{
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, true);
    qof_book_destroy (m_book);
    m_book = nullptr;
    m_book_root = nullptr;
    m_account = nullptr;
    m_child = nullptr;
    m_grandchild = nullptr;

    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (find_cascade_dialog (), nullptr);
}

TEST_F (AccountCascadeResponseTest, ReentrantAccountEvent)
{
    register_event_handler ();
    auto dialog = start_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    select_all_updates (dialog, true);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    unregister_event_handler ();

    EXPECT_TRUE (m_removal.removed);
    EXPECT_TRUE (xaccAccountGetHidden (m_account));
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for account-cascade response tests");
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    return RUN_ALL_TESTS ();
}
