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

TEST (ObjectReferencesConstructionTest, PreservesTemplatePresentation)
{
    auto dialog = GTK_WIDGET (g_object_new (GNC_TYPE_OBJECT_REFERENCES_DIALOG, nullptr));
    EXPECT_STREQ (gtk_window_get_title (GTK_WINDOW (dialog)), "Object references");
    EXPECT_STREQ (gtk_widget_get_name (dialog), "gnc-id-object-reference");
    EXPECT_NE (gtk_dialog_get_widget_for_response (GTK_DIALOG (dialog), 0), nullptr);
    gtk_widget_destroy (dialog);
}
}
