/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"
#include <cstdlib>

#include "qof.h"
#include "gnc-amount-edit.h"
#include "gnc-exp-parser.h"

/* These C headers do not yet provide C++ linkage guards. */
extern "C"
{
#include "search-double.h"
#include "search-int64.h"
#include "search-numeric.h"
}

namespace
{
static GtkWidget *
find_amount (GtkWidget *widget)
{
    if (GNC_IS_AMOUNT_EDIT (widget))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_amount (GTK_WIDGET (node->data));
    g_list_free (children);
    return found;
}

static GtkWidget *
find_notice (GtkWidget *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data) &&
            (!parent || gtk_window_get_transient_for (GTK_WINDOW (node->data)) == GTK_WINDOW (parent)))
        {
            EXPECT_EQ (found, nullptr);
            found = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return found;
}

class NumberSearchResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    }

    void TearDown () override
    {
        if (parent)
            gtk_widget_destroy (parent);
    }

    GtkWidget *parent{};

    void check_valid_then_invalid_value (GNCSearchCoreType *core)
    {
        gnc_search_core_type_pass_parent (core, parent);
        auto widget = gnc_search_core_type_get_widget (core);
        gtk_container_add (GTK_CONTAINER (parent), widget);
        auto amount = find_amount (widget);
        ASSERT_NE (amount, nullptr);
        auto entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (amount));
        gtk_entry_set_text (GTK_ENTRY (entry), "1");
        EXPECT_TRUE (gnc_search_core_type_validate (core));
        EXPECT_EQ (find_notice (parent), nullptr);
        gtk_entry_set_text (GTK_ENTRY (entry), "(");
        EXPECT_FALSE (gnc_search_core_type_validate (core));
    }

    GtkWidget *notice_for_parent ()
    {
        auto notice = find_notice (parent);
        EXPECT_NE (notice, nullptr);
        if (!notice)
            return nullptr;
        EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (notice)));
        EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (notice)));
        return notice;
    }

    void destroy_validator (GtkWidget *widget, GNCSearchCoreType *core)
    {
        gtk_widget_destroy (widget);
        g_object_unref (core);
    }
};

TEST_F (NumberSearchResponseTest, DoubleSearchClosesNoticeAfterValidatorIsDestroyed)
{
    auto core = GNC_SEARCH_CORE_TYPE (gnc_search_double_new ());
    check_valid_then_invalid_value (core);
    auto widget = gnc_search_core_type_get_widget (core);
    auto notice = notice_for_parent ();
    ASSERT_NE (notice, nullptr);
    destroy_validator (widget, core);
    gtk_dialog_response (GTK_DIALOG (notice), GTK_RESPONSE_CLOSE);
    EXPECT_EQ (find_notice (parent), nullptr);
}

TEST_F (NumberSearchResponseTest, Int64SearchClosesNoticeAfterValidatorIsDestroyed)
{
    auto core = GNC_SEARCH_CORE_TYPE (gnc_search_int64_new ());
    check_valid_then_invalid_value (core);
    auto widget = gnc_search_core_type_get_widget (core);
    auto notice = notice_for_parent ();
    ASSERT_NE (notice, nullptr);
    destroy_validator (widget, core);
    gtk_dialog_response (GTK_DIALOG (notice), GTK_RESPONSE_CLOSE);
    EXPECT_EQ (find_notice (parent), nullptr);
}

TEST_F (NumberSearchResponseTest, NumericSearchClosesNoticeAfterValidatorIsDestroyed)
{
    auto core = GNC_SEARCH_CORE_TYPE (gnc_search_numeric_new ());
    check_valid_then_invalid_value (core);
    auto widget = gnc_search_core_type_get_widget (core);
    auto notice = notice_for_parent ();
    ASSERT_NE (notice, nullptr);
    destroy_validator (widget, core);
    gtk_dialog_response (GTK_DIALOG (notice), GTK_RESPONSE_CLOSE);
    EXPECT_EQ (find_notice (parent), nullptr);
}

TEST_F (NumberSearchResponseTest, DoubleSearchNoticeClosesWithParentAfterValidatorDestruction)
{
    auto core = GNC_SEARCH_CORE_TYPE (gnc_search_double_new ());
    check_valid_then_invalid_value (core);
    auto widget = gnc_search_core_type_get_widget (core);
    auto notice = notice_for_parent ();
    ASSERT_NE (notice, nullptr);
    destroy_validator (widget, core);
    gtk_widget_destroy (parent);
    parent = nullptr;
    EXPECT_EQ (find_notice (nullptr), nullptr);
}

TEST_F (NumberSearchResponseTest, Int64SearchNoticeClosesWithParentAfterValidatorDestruction)
{
    auto core = GNC_SEARCH_CORE_TYPE (gnc_search_int64_new ());
    check_valid_then_invalid_value (core);
    auto widget = gnc_search_core_type_get_widget (core);
    auto notice = notice_for_parent ();
    ASSERT_NE (notice, nullptr);
    destroy_validator (widget, core);
    gtk_widget_destroy (parent);
    parent = nullptr;
    EXPECT_EQ (find_notice (nullptr), nullptr);
}

TEST_F (NumberSearchResponseTest, NumericSearchNoticeClosesWithParentAfterValidatorDestruction)
{
    auto core = GNC_SEARCH_CORE_TYPE (gnc_search_numeric_new ());
    check_valid_then_invalid_value (core);
    auto widget = gnc_search_core_type_get_widget (core);
    auto notice = notice_for_parent ();
    ASSERT_NE (notice, nullptr);
    destroy_validator (widget, core);
    gtk_widget_destroy (parent);
    parent = nullptr;
    EXPECT_EQ (find_notice (nullptr), nullptr);
}
}

static int
run_tests (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    qof_init ();
    /* Parser initialization loads fin.scm even for literal expressions.
     * Guile must be initialized; saved user variables are not needed. */
    gnc_exp_parser_real_init (FALSE);
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    gnc_exp_parser_shutdown ();
    qof_close ();
    return result;
}

static void
guile_main ([[maybe_unused]] void *data, int argc, char **argv)
{
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed for numeric search response tests.\n");
        std::exit (1);
    }
    std::exit (run_tests (argc, argv));
}

int
main (int argc, char **argv)
{
    scm_boot_guile (argc, argv, guile_main, nullptr);
    return 0;
}
