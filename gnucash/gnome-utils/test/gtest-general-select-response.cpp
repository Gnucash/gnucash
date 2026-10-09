/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
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
#include "googletest-glib-log-handler.hpp"

#include "gnc-general-select.h"

namespace
{
int old_selection;
int new_selection;
GtkWidget *destroy_from_get_string;

struct Selector
{
    std::uint32_t calls = 0;
    bool complete_inline = false;
    gpointer inline_selection = nullptr;
    GNCGeneralSelectAsyncResultCB completed = nullptr;
    gpointer completion_data = nullptr;
};

static const char *
get_string (gpointer selection)
{
    if (destroy_from_get_string)
    {
        auto widget = destroy_from_get_string;
        destroy_from_get_string = nullptr;
        gtk_widget_destroy (widget);
    }
    if (selection == &old_selection)
        return "Old selection";
    if (selection == &new_selection)
        return "New selection";
    return "Unknown selection";
}

static void
select_async ([[maybe_unused]] gpointer cb_arg, gpointer,
              [[maybe_unused]] GtkWidget *parent,
              GNCGeneralSelectAsyncResultCB completed, gpointer user_data)
{
    auto selector = static_cast<Selector *> (cb_arg);
    ++selector->calls;
    if (selector->complete_inline)
    {
        completed (selector->inline_selection, user_data);
        return;
    }
    selector->completed = completed;
    selector->completion_data = user_data;
}

static void
complete_selection (Selector &selector, gpointer selection)
{
    auto completed = selector.completed;
    auto data = selector.completion_data;
    selector.completed = nullptr;
    selector.completion_data = nullptr;
    EXPECT_NE (completed, nullptr);
    if (!completed)
        return;
    completed (selection, data);
}

static void
count_changed (GNCGeneralSelect *, gpointer data)
{
    ++*static_cast<std::uint32_t *> (data);
}

static GtkWidget *
create_select (GtkWidget *parent, Selector &selector)
{
    auto widget = gnc_general_select_new_async (
        GNC_GENERAL_SELECT_TYPE_SELECT, get_string, select_async, &selector);
    gtk_container_add (GTK_CONTAINER (parent), widget);
    return widget;
}

class GeneralSelectResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        destroy_from_get_string = nullptr;
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (parent);
        widget = create_select (parent, selector);
        g_object_ref (widget);
        select = GNC_GENERAL_SELECT (widget);
        g_signal_connect (select, "changed", G_CALLBACK (count_changed), &changed);
    }

    void TearDown () override
    {
        gtk_widget_destroy (parent);
        if (selector.completed)
            complete_selection (selector, nullptr);
        g_object_unref (widget);
        g_object_unref (parent);
        destroy_from_get_string = nullptr;
    }

    Selector selector{};
    GtkWidget *parent{};
    GtkWidget *widget{};
    GNCGeneralSelect *select{};
    std::uint32_t changed{};
};
TEST_F (GeneralSelectResponseTest, PendingCancelUpdateAndInlineCompletion)
{
    gnc_general_select_set_selected (select, &old_selection);
    EXPECT_EQ (gnc_general_select_get_selected (select), &old_selection);
    changed = 0;

    gtk_button_clicked (GTK_BUTTON (select->button));
    gtk_button_clicked (GTK_BUTTON (select->button));
    EXPECT_EQ (selector.calls, 1u);
    complete_selection (selector, nullptr);
    EXPECT_EQ (gnc_general_select_get_selected (select), &old_selection);
    EXPECT_EQ (changed, 0u);

    gtk_button_clicked (GTK_BUTTON (select->button));
    EXPECT_EQ (selector.calls, 2u);
    complete_selection (selector, &new_selection);
    EXPECT_EQ (gnc_general_select_get_selected (select), &new_selection);
    EXPECT_STREQ (gtk_entry_get_text (GTK_ENTRY (select->entry)), "New selection");
    EXPECT_EQ (changed, 1u);

    selector.complete_inline = true;
    selector.inline_selection = &old_selection;
    gtk_button_clicked (GTK_BUTTON (select->button));
    EXPECT_EQ (selector.calls, 3u);
    EXPECT_EQ (gnc_general_select_get_selected (select), &old_selection);
    EXPECT_EQ (changed, 2u);
}

static void
destroy_on_entry_changed (GtkEditable *, gpointer data)
{
    gtk_widget_destroy (GTK_WIDGET (data));
}

TEST_F (GeneralSelectResponseTest, LateCompletionAfterParentDestroyIsIgnored)
{

    gtk_button_clicked (GTK_BUTTON (select->button));
    gtk_widget_destroy (parent);
    complete_selection (selector, &new_selection);
    EXPECT_EQ (gnc_general_select_get_selected (select), nullptr);
    EXPECT_EQ (changed, 0u);
}

TEST_F (GeneralSelectResponseTest, EntryNotificationMayDestroyWidget)
{
    g_signal_connect (select->entry, "changed",
                      G_CALLBACK (destroy_on_entry_changed), select);
    gtk_widget_show_all (parent);
    gnc_general_select_set_selected (select, &new_selection);
    EXPECT_EQ (gnc_general_select_get_selected (select), nullptr);
    EXPECT_EQ (changed, 0u);
}

TEST_F (GeneralSelectResponseTest, StringLookupMayDestroyWidget)
{
    destroy_from_get_string = widget;
    gnc_general_select_set_selected (select, &new_selection);
    EXPECT_EQ (gnc_general_select_get_selected (select), nullptr);
    EXPECT_EQ (changed, 0u);
    gtk_widget_destroy (parent);
}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for general-select tests");
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
