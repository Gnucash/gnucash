/* Tests for object reference dialog construction and responses.
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
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
TEST (ObjectReferencesConstructionTest, PreservesTemplatePresentation)
{
    auto dialog = GTK_WIDGET (g_object_new (GNC_TYPE_OBJECT_REFERENCES_DIALOG, nullptr));
    EXPECT_STREQ (gtk_window_get_title (GTK_WINDOW (dialog)), "Object references");
    EXPECT_STREQ (gtk_widget_get_name (dialog), "gnc-id-object-reference");
    EXPECT_NE (gtk_dialog_get_widget_for_response (GTK_DIALOG (dialog), 0), nullptr);
    gtk_widget_destroy (dialog);
}
}
