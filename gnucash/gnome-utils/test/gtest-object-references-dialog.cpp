/* Tests for object reference dialog construction and responses.
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include <glib/gstdio.h>
#include <gtest/gtest.h>
#include <cstdlib>
#include "dialog-object-references.h"

namespace
{
class GtkEnvironment : public ::testing::Environment
{
    void SetUp () override
    {
        ASSERT_TRUE (gtk_init_check (nullptr, nullptr));
    }
};
[[maybe_unused]] auto gtk_environment = ::testing::AddGlobalTestEnvironment (new GtkEnvironment);

GtkWidget *
find_child (GtkWidget *widget, GType type)
{
    if (g_type_is_a (G_OBJECT_TYPE (widget), type))
        return widget;
    if (!GTK_IS_CONTAINER (widget) || (type == GTK_TYPE_LABEL && GTK_IS_BUTTON (widget)))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_child (GTK_WIDGET (node->data), type);
    g_list_free (children);
    return result;
}

class ObjectReferencesDialogTest : public ::testing::Test
{
protected:
    GtkWidget *dialog = nullptr;
    void TearDown () override
    {
        if (dialog)
            gtk_widget_destroy (dialog);
    }
    void create (const char * const *names)
    {
        dialog = GTK_WIDGET (g_object_new (GNC_TYPE_OBJECT_REFERENCES_DIALOG,
                                          "explanation", "These objects refer to this account.",
                                          "object-names", names, nullptr));
    }
};

TEST (ObjectReferencesTemplateTest, MissingTemplateDoesNotAbortOrShowDialog)
{
    ::testing::FLAGS_gtest_death_test_style = "threadsafe";
    EXPECT_EXIT (
        {
            auto directory = g_dir_make_tmp ("gnc-object-references-XXXXXX", nullptr);
            g_setenv ("GNC_UNINSTALLED", "YES", TRUE);
            g_setenv ("GNC_BUILDDIR", directory, TRUE);
            g_test_expect_message ("gnc.gui", G_LOG_LEVEL_WARNING, "*Could not load object references dialog:*");
            gnc_ui_object_references_show ("Unavailable template", nullptr);
            g_test_assert_expected_messages ();
            auto windows = gtk_window_list_toplevels ();
            bool empty = windows == nullptr;
            g_list_free (windows);
            g_rmdir (directory);
            g_free (directory);
            std::exit (empty ? 0 : 1);
        },
        ::testing::ExitedWithCode (0), "");
}

TEST_F (ObjectReferencesDialogTest, DisplaysExplanationAndOrderedNames)
{
    const char *names[] = {"Invoice B", "Rechnung Ä", "Invoice B", nullptr};
    create (names);
    auto label = find_child (dialog, GTK_TYPE_LABEL);
    ASSERT_NE (label, nullptr);
    EXPECT_STREQ (gtk_label_get_text (GTK_LABEL (label)), "These objects refer to this account.");
    auto view = find_child (dialog, GTK_TYPE_TREE_VIEW);
    ASSERT_NE (view, nullptr);
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    ASSERT_EQ (gtk_tree_model_iter_n_children (model, nullptr), 3);
    GtkTreeIter iter;
    ASSERT_TRUE (gtk_tree_model_get_iter_first (model, &iter));
    for (const auto expected : {"Invoice B", "Rechnung Ä", "Invoice B"})
    {
        gchar *actual = nullptr;
        gtk_tree_model_get (model, &iter, 0, &actual, -1);
        EXPECT_STREQ (actual, expected);
        g_free (actual);
        gtk_tree_model_iter_next (model, &iter);
    }
    EXPECT_STREQ (gtk_window_get_title (GTK_WINDOW (dialog)), "Object references");
    EXPECT_STREQ (gtk_widget_get_name (dialog), "gnc-id-object-reference");
    EXPECT_NE (gtk_dialog_get_widget_for_response (GTK_DIALOG (dialog), 0), nullptr);
}

TEST_F (ObjectReferencesDialogTest, CopiesNamesBeforeCallerReleasesThem)
{
    auto names = g_new0 (gchar*, 2);
    names[0] = g_strdup ("Owned row");
    create (names);
    g_strfreev (names);
    auto view = find_child (dialog, GTK_TYPE_TREE_VIEW);
    ASSERT_NE (view, nullptr);
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gtk_tree_model_get_iter_first (model, &iter));
    gchar *actual = nullptr;
    gtk_tree_model_get (model, &iter, 0, &actual, -1);
    EXPECT_STREQ (actual, "Owned row");
    g_free (actual);
}

TEST_F (ObjectReferencesDialogTest, EmptyList)
{
    const char *names[] = {nullptr};
    create (names);
    auto view = find_child (dialog, GTK_TYPE_TREE_VIEW);
    ASSERT_NE (view, nullptr);
    EXPECT_EQ (gtk_tree_model_iter_n_children (gtk_tree_view_get_model (GTK_TREE_VIEW (view)), nullptr), 0);
}

GtkWidget *
find_reference_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GNC_IS_OBJECT_REFERENCES_DIALOG (node->data))
            found = GTK_WIDGET (node->data);
    g_list_free (windows);
    return found;
}

static gboolean
close_nested_dialog (gpointer data)
{
    *static_cast<bool*> (data) = true;
    if (auto dialog = find_reference_dialog ())
        gtk_dialog_response (GTK_DIALOG (dialog), 0);
    return G_SOURCE_REMOVE;
}

static void
count_destroy ([[maybe_unused]] GtkWidget *widget, gpointer data)
{
    ++*static_cast<unsigned*> (data);
}

class ObjectReferencesPresentationTest : public ::testing::Test
{
protected:
    GtkWidget *dialog = nullptr;
    unsigned destroy_count = 0;

    bool show ()
    {
        bool nested = false;
        auto source = g_idle_add (close_nested_dialog, &nested);
        gnc_ui_object_references_show ("Referenced objects", nullptr);
        if (!nested)
            g_source_remove (source);
        dialog = find_reference_dialog ();
        if (dialog)
        {
            g_object_ref (dialog);
            g_signal_connect (dialog, "destroy", G_CALLBACK (count_destroy), &destroy_count);
        }
        return !nested;
    }

    void TearDown () override
    {
        if (dialog)
        {
            if (!destroy_count)
                gtk_widget_destroy (dialog);
            g_object_unref (dialog);
        }
    }
};

TEST_F (ObjectReferencesPresentationTest, ReturnsBeforeResponse)
{
    ASSERT_TRUE (show ());
    ASSERT_NE (dialog, nullptr);
    EXPECT_TRUE (gtk_widget_get_visible (dialog));
    EXPECT_EQ (destroy_count, 0u);
}

TEST_F (ObjectReferencesPresentationTest, ModalAfterShow)
{
    ASSERT_TRUE (show ());
    ASSERT_NE (dialog, nullptr);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
}

TEST_F (ObjectReferencesPresentationTest, OkDestroysOnce)
{
    ASSERT_TRUE (show ());
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), 0);
    EXPECT_EQ (destroy_count, 1u);
    EXPECT_EQ (find_reference_dialog (), nullptr);
}

TEST_F (ObjectReferencesPresentationTest, WindowCloseDestroysOnce)
{
    ASSERT_TRUE (show ());
    ASSERT_NE (dialog, nullptr);
    gtk_window_close (GTK_WINDOW (dialog));
    for (unsigned attempt = 0; !destroy_count && attempt < 100; ++attempt)
    {
        g_main_context_iteration (nullptr, FALSE);
        g_usleep (1000);
    }
    EXPECT_EQ (destroy_count, 1u);
    EXPECT_EQ (find_reference_dialog (), nullptr);
}

TEST (ObjectReferencesConstructionTest, PreservesTemplatePresentation)
{
    auto dialog = GTK_WIDGET (g_object_new (GNC_TYPE_OBJECT_REFERENCES_DIALOG, nullptr));
    EXPECT_STREQ (gtk_window_get_title (GTK_WINDOW (dialog)), "Object references");
    EXPECT_STREQ (gtk_widget_get_name (dialog), "gnc-id-object-reference");
    EXPECT_NE (gtk_dialog_get_widget_for_response (GTK_DIALOG (dialog), 0), nullptr);
    gtk_widget_destroy (dialog);
}
}
