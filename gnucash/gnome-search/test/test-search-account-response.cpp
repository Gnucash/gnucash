/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>

#include "Account.h"
#include "cashobjects.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "qof.h"
#include "search-account.h"
#include "search-core-type.h"

namespace
{
static GtkWidget *
find_widget (GtkWidget *root, bool (*match)(GtkWidget *))
{
    if (match (root))
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_widget (GTK_WIDGET (node->data), match);
    g_list_free (children);
    return found;
}

static bool is_button (GtkWidget *widget) { return GTK_IS_BUTTON (widget); }
static bool is_account_view (GtkWidget *widget)
{
    return GNC_IS_TREE_VIEW_ACCOUNT (widget);
}

static const gchar *
button_text (GtkWidget *button)
{
    auto label = gtk_bin_get_child (GTK_BIN (button));
    return GTK_IS_LABEL (label) ? gtk_label_get_text (GTK_LABEL (label)) : nullptr;
}

static GtkWidget *
find_selection_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            g_strcmp0 (gtk_window_get_title (GTK_WINDOW (widget)),
                       "Select the Accounts to Compare") == 0)
            found = widget;
    }
    g_list_free (windows);
    return found;
}

class AccountSearchResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        book = qof_book_new ();
        auto root = gnc_account_create_root (book);
        account = xaccMallocAccount (book);
        xaccAccountSetName (account, "Search selection target");
        gnc_account_append_child (root, account);
        session = qof_session_new (book);
        gnc_set_current_session (session);

        parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        contents = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
        gtk_container_add (GTK_CONTAINER (parent), contents);
        search = gnc_search_account_new ();
        gnc_search_core_type_pass_parent (GNC_SEARCH_CORE_TYPE (search),
                                          GTK_WIDGET (parent));
        auto widget = gnc_search_core_type_get_widget (
            GNC_SEARCH_CORE_TYPE (search));
        gtk_container_add (GTK_CONTAINER (contents), widget);
        button = find_widget (widget, is_button);
        ASSERT_NE (button, nullptr);
        ASSERT_TRUE (GTK_IS_BUTTON (button));
    }

    void TearDown () override
    {
        if (parent)
            gtk_widget_destroy (GTK_WIDGET (parent));
        g_object_unref (search);
        gnc_clear_current_session ();
    }

    GtkWidget *open_dialog ()
    {
        gtk_button_clicked (GTK_BUTTON (button));
        auto dialog = find_selection_dialog ();
        EXPECT_NE (dialog, nullptr);
        if (dialog)
        {
            EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
            EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
            EXPECT_NE (find_widget (dialog, is_account_view), nullptr);
        }
        return dialog;
    }

    QofBook *book{};
    QofSession *session{};
    Account *account{};
    GtkWindow *parent{};
    GtkWidget *contents{};
    GNCSearchAccount *search{};
    GtkWidget *button{};
};

TEST_F (AccountSearchResponseTest, AcceptsSelectedAccountAndCancelPreservesSelection)
{
    auto dialog = open_dialog ();
    ASSERT_NE (dialog, nullptr);
    auto view = find_widget (dialog, is_account_view);
    GList selected{account, nullptr, nullptr};
    gnc_tree_view_account_set_selected_accounts (
        GNC_TREE_VIEW_ACCOUNT (view), &selected, false);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    auto predicate = gnc_search_core_type_get_predicate (
        GNC_SEARCH_CORE_TYPE (search));
    ASSERT_NE (predicate, nullptr);
    qof_query_core_predicate_free (predicate);
    EXPECT_STREQ (button_text (button), "Selected Accounts");

    dialog = open_dialog ();
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    EXPECT_STREQ (button_text (button), "Selected Accounts");
}

TEST_F (AccountSearchResponseTest, OwnerMayBeDestroyedBeforeDialogResponse)
{
    auto owner = gnc_search_account_new ();
    gnc_search_core_type_pass_parent (GNC_SEARCH_CORE_TYPE (owner),
                                      GTK_WIDGET (parent));
    auto owner_widget = gnc_search_core_type_get_widget (
        GNC_SEARCH_CORE_TYPE (owner));
    gtk_container_add (GTK_CONTAINER (contents), owner_widget);
    auto owner_button = find_widget (owner_widget, is_button);
    ASSERT_NE (owner_button, nullptr);
    gtk_button_clicked (GTK_BUTTON (owner_button));
    auto dialog = find_selection_dialog ();
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    g_object_unref (owner);

    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_STREQ (button_text (owner_button), "Choose Accounts");
    g_object_unref (dialog);
}

TEST_F (AccountSearchResponseTest, LateResponseAfterParentDestructionKeepsRetainedLabelSafe)
{
    auto dialog = open_dialog ();
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    auto label = gtk_bin_get_child (GTK_BIN (button));
    ASSERT_NE (label, nullptr);
    g_object_ref (label);

    gtk_widget_destroy (GTK_WIDGET (parent));
    parent = nullptr;
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_STREQ (gtk_label_get_text (GTK_LABEL (label)), "Choose Accounts");

    g_object_unref (label);
    g_object_unref (dialog);
}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed for account search response tests.\n");
        return 1;
    }
    qof_init ();
    if (!cashobjects_register ())
    {
        g_printerr ("Could not register cash objects for account search tests.\n");
        qof_close ();
        return 1;
    }
    gnc_gsettings_load_backend ();
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto result = RUN_ALL_TESTS ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return result;
}
