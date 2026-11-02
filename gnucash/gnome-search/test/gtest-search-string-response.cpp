/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"

extern "C"
{
#include "search-string.h"
}

namespace
{
static GtkWidget *
find_warning ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto window = windows; window; window = window->next)
        if (GTK_IS_MESSAGE_DIALOG (window->data))
        {
            EXPECT_EQ (dialog, nullptr);
            dialog = GTK_WIDGET (window->data);
        }
    g_list_free (windows);
    return dialog;
}

class StringSearchResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        search = gnc_search_string_new ();
        gnc_search_core_type_pass_parent (GNC_SEARCH_CORE_TYPE (search), parent);
    }

    void TearDown () override
    {
        if (search)
            g_object_unref (search);
        if (parent)
            gtk_widget_destroy (parent);
        EXPECT_EQ (find_warning (), nullptr);
    }

    GtkWidget *parent{};
    GNCSearchString *search{};

    GtkWidget *show_invalid_value (const char *value, bool regex)
    {
        gnc_search_string_set_value (search, value);
        if (regex)
            gnc_search_string_set_how (search, SEARCH_STRING_MATCHES_REGEX);
        EXPECT_FALSE (gnc_search_core_type_validate (
            GNC_SEARCH_CORE_TYPE (search)));
        auto dialog = find_warning ();
        EXPECT_NE (dialog, nullptr);
        if (!dialog)
            return nullptr;
        EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
        EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
        return dialog;
    }

    void release_validator_after_warning (GtkWidget *dialog)
    {
        /* The notice owns its text, not the validator or its input storage. */
        gnc_search_string_set_value (search, "valid");
        EXPECT_TRUE (gnc_search_core_type_validate (
            GNC_SEARCH_CORE_TYPE (search)));
        g_object_unref (search);
        search = nullptr;
        EXPECT_NE (dialog, nullptr);
    }
};

TEST_F (StringSearchResponseTest, RejectsEmptyValueAndClosesOnResponse)
{
    auto dialog = show_invalid_value ("", false);
    ASSERT_NE (dialog, nullptr);
    release_validator_after_warning (dialog);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (find_warning (), nullptr);
}

TEST_F (StringSearchResponseTest, RejectsInvalidRegularExpressionAndClosesOnResponse)
{
    auto dialog = show_invalid_value ("[", true);
    ASSERT_NE (dialog, nullptr);
    release_validator_after_warning (dialog);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (find_warning (), nullptr);
}

TEST_F (StringSearchResponseTest, WarningClosesWhenParentIsDestroyed)
{
    auto dialog = show_invalid_value ("[", true);
    ASSERT_NE (dialog, nullptr);
    release_validator_after_warning (dialog);
    gtk_widget_destroy (parent);
    parent = nullptr;
    EXPECT_EQ (find_warning (), nullptr);
}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed for string search response tests.\n");
        return 1;
    }
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
