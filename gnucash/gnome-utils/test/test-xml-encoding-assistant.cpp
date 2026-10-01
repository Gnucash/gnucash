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
#include <glib/gstdio.h>
#include <gtest/gtest.h>

/* Include the QOF declarations as C++ first, then give the public C entrypoint
   C linkage. assistant-xml-encoding.h itself has no extern-C guard. */
#include "qof.h"
extern "C"
{
#include "assistant-xml-encoding.h"
}
#include "cashobjects.h"
#include "gnc-session.h"
#include "qofbook.h"
#include "qofsession.h"

namespace
{
constexpr char xml_fixture[] = "<gnc>\xc3\xa9</gnc>\n";

struct ImportResult
{
    bool completed{};
    bool converted{};
    bool driver_ran{};
    std::uint32_t driver_source_id{};
    std::uint32_t dialog_destroy_count{};
    GtkWidget *assistant{};
    GWeakRef dialog_ref{};
    GWeakRef error_ref{};
};

ImportResult *active_result{};

struct EncodingPath
{
    const char *encoding;
    GtkTreePath *path;
};

static gboolean
find_encoding_path (GtkTreeModel *model, GtkTreePath *path,
                    GtkTreeIter *iter, gpointer user_data)
{
    auto wanted = static_cast<EncodingPath *> (user_data);
    gpointer quark_ptr = nullptr;
    EXPECT_EQ (gtk_tree_model_get_column_type (model, 1), G_TYPE_POINTER);
    gtk_tree_model_get (model, iter, 1, &quark_ptr, -1);
    auto encoding = g_quark_to_string (GPOINTER_TO_UINT (quark_ptr));
    if (g_strcmp0 (encoding, wanted->encoding) != 0)
        return false;
    wanted->path = gtk_tree_path_copy (path);
    return true;
}

static GtkWidget *
find_named_child (GtkWidget *root, const char *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;

    if (!GTK_IS_CONTAINER (root))
        return nullptr;

    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_named_child (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return result;
}

static GtkWidget *
find_assistant ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        if (GTK_IS_ASSISTANT (node->data))
        {
            EXPECT_EQ (result, nullptr);
            result = GTK_WIDGET (node->data);
        }
    }
    g_list_free (windows);
    return result;
}

static GtkWidget *
find_encoding_dialog (GtkWidget *assistant)
{
    if (!assistant)
        return nullptr;
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) ==
            GTK_WINDOW (assistant))
        {
            EXPECT_EQ (result, nullptr);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

static void
track_dialog_destruction (GtkWidget *dialog)
{
    g_weak_ref_set (&active_result->dialog_ref, G_OBJECT (dialog));
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (+[] (GtkWidget *, gpointer data)
                      {
                          ++*static_cast<std::uint32_t *> (data);
                      }), &active_result->dialog_destroy_count);
}

static std::uint32_t
row_count (GtkWidget *view)
{
    return gtk_tree_model_iter_n_children (
        gtk_tree_view_get_model (GTK_TREE_VIEW (view)), nullptr);
}

static void
remove_latin1_encoding (GtkWidget *dialog)
{
    auto view = find_named_child (dialog, "selected_encs_view");
    auto remove = find_named_child (dialog, "remove_enc_button");
    EXPECT_NE (view, nullptr);
    EXPECT_NE (remove, nullptr);
    if (!view || !remove)
        return;

    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    EncodingPath wanted {"ISO-8859-1", nullptr};
    gtk_tree_model_foreach (model, find_encoding_path, &wanted);
    EXPECT_NE (wanted.path, nullptr);
    if (!wanted.path)
        return;
    gtk_tree_selection_select_path (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), wanted.path);
    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), wanted.path, nullptr, false);
    gtk_tree_path_free (wanted.path);
    gtk_button_clicked (GTK_BUTTON (remove));
}

static void
add_latin1_encoding (GtkWidget *dialog)
{
    auto view = find_named_child (dialog, "available_encs_view");
    auto add = find_named_child (dialog, "add_enc_button");
    EXPECT_NE (view, nullptr);
    EXPECT_NE (add, nullptr);
    if (!view || !add)
        return;
    EncodingPath wanted {"ISO-8859-1", nullptr};
    gtk_tree_model_foreach (gtk_tree_view_get_model (GTK_TREE_VIEW (view)),
                            find_encoding_path, &wanted);
    EXPECT_NE (wanted.path, nullptr);
    if (!wanted.path)
        return;
    gtk_tree_view_expand_to_path (GTK_TREE_VIEW (view), wanted.path);
    gtk_tree_selection_select_path (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), wanted.path);
    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), wanted.path, nullptr, false);
    EXPECT_TRUE (gtk_tree_selection_path_is_selected (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), wanted.path));
    gtk_tree_path_free (wanted.path);
    gtk_button_clicked (GTK_BUTTON (add));
}

static void
check_rejected_encoding (GtkWidget *dialog, const char *encoding)
{
    auto selected = find_named_child (dialog, "selected_encs_view");
    auto entry = find_named_child (dialog, "custom_enc_entry");
    EXPECT_TRUE (GTK_IS_ENTRY (entry));
    if (!selected || !GTK_IS_ENTRY (entry))
        return;
    auto count = row_count (selected);
    gtk_entry_set_text (GTK_ENTRY (entry), encoding);
    auto add = find_named_child (dialog, "add_custom_enc_button");
    EXPECT_TRUE (GTK_IS_BUTTON (add));
    if (!GTK_IS_BUTTON (add))
        return;
    gtk_button_clicked (GTK_BUTTON (add));
    auto error = find_encoding_dialog (dialog);
    EXPECT_TRUE (GTK_IS_MESSAGE_DIALOG (error));
    if (!GTK_IS_MESSAGE_DIALOG (error))
        return;
    EXPECT_EQ (row_count (selected), count);
    gtk_dialog_response (GTK_DIALOG (error), GTK_RESPONSE_CLOSE);
    EXPECT_EQ (find_encoding_dialog (dialog), nullptr);
}

static gboolean
drive_cancel_then_parent_cancel (gpointer)
{
    active_result->driver_source_id = 0;
    active_result->driver_ran = true;
    active_result->assistant = find_assistant ();
    EXPECT_NE (active_result->assistant, nullptr);
    if (!active_result->assistant)
        return G_SOURCE_REMOVE;
    gtk_assistant_set_current_page (GTK_ASSISTANT (active_result->assistant), 1);

    auto edit = find_named_child (active_result->assistant, "edit_encs_button");
    EXPECT_NE (edit, nullptr);
    if (!edit)
        return G_SOURCE_REMOVE;
    gtk_button_clicked (GTK_BUTTON (edit));
    auto dialog = find_encoding_dialog (active_result->assistant);
    EXPECT_NE (dialog, nullptr);
    if (!dialog)
        return G_SOURCE_REMOVE;
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto selected = find_named_child (dialog, "selected_encs_view");
    EXPECT_NE (selected, nullptr);
    if (!selected)
        return G_SOURCE_REMOVE;
    auto original_count = row_count (selected);
    check_rejected_encoding (dialog, "ISO-8859-1");
    check_rejected_encoding (dialog, "gnc-invalid-test-encoding");
    remove_latin1_encoding (dialog);
    EXPECT_EQ (row_count (selected), original_count - 1);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (find_encoding_dialog (active_result->assistant), nullptr);

    gtk_button_clicked (GTK_BUTTON (edit));
    dialog = find_encoding_dialog (active_result->assistant);
    EXPECT_NE (dialog, nullptr);
    if (!dialog)
        return G_SOURCE_REMOVE;
    selected = find_named_child (dialog, "selected_encs_view");
    EXPECT_EQ (row_count (selected), original_count);
    remove_latin1_encoding (dialog);
    EXPECT_EQ (row_count (selected), original_count - 1);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    gtk_button_clicked (GTK_BUTTON (edit));
    dialog = find_encoding_dialog (active_result->assistant);
    EXPECT_NE (dialog, nullptr);
    if (!dialog)
        return G_SOURCE_REMOVE;
    track_dialog_destruction (dialog);
    selected = find_named_child (dialog, "selected_encs_view");
    EXPECT_EQ (row_count (selected), original_count - 1);
    add_latin1_encoding (dialog);
    EXPECT_EQ (row_count (selected), original_count);
    auto entry = find_named_child (dialog, "custom_enc_entry");
    gtk_entry_set_text (GTK_ENTRY (entry), "gnc-invalid-test-encoding");
    auto add = find_named_child (dialog, "add_custom_enc_button");
    EXPECT_TRUE (GTK_IS_BUTTON (add));
    gtk_button_clicked (GTK_BUTTON (add));
    auto error = find_encoding_dialog (dialog);
    EXPECT_TRUE (GTK_IS_MESSAGE_DIALOG (error));
    if (!GTK_IS_MESSAGE_DIALOG (error))
        return G_SOURCE_REMOVE;
    g_weak_ref_set (&active_result->error_ref, G_OBJECT (error));

    /* Closing the parent while the editor is open must dispose the child and
       roll back its unaccepted working list before importer state is freed. */
    g_signal_emit_by_name (active_result->assistant, "cancel");
    return G_SOURCE_REMOVE;
}

class XmlEncodingAssistantTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        gnc_set_current_session (qof_session_new (qof_book_new ()));
        active_result = &result;
        g_weak_ref_init (&result.dialog_ref, nullptr);
        g_weak_ref_init (&result.error_ref, nullptr);
        GError *error = nullptr;
        const std::int32_t fd = g_file_open_tmp ("gnc-xml-encoding-XXXXXX", &filename,
                                         &error);
        ASSERT_GE (fd, 0) << (error ? error->message : "");
        g_clear_error (&error);
        ASSERT_TRUE (g_close (fd, &error));
        g_clear_error (&error);
        ASSERT_TRUE (g_file_set_contents (filename, xml_fixture,
                                          sizeof (xml_fixture) - 1, &error))
            << (error ? error->message : "");
        g_clear_error (&error);
        uri = g_filename_to_uri (filename, nullptr, &error);
        ASSERT_NE (uri, nullptr) << (error ? error->message : "");
        g_clear_error (&error);
    }
    void TearDown () override
    {
        if (result.driver_source_id)
        {
            g_source_remove (result.driver_source_id);
            result.driver_source_id = 0;
        }
        g_weak_ref_clear (&result.dialog_ref);
        g_weak_ref_clear (&result.error_ref);
        if (filename)
        {
            g_remove (filename);
            g_free (filename);
            filename = nullptr;
        }
        g_clear_pointer (&uri, g_free);
        gnc_clear_current_session ();
        active_result = nullptr;
    }
    gchar *filename{};
    gchar *uri{};
    ImportResult result{};
};

TEST_F (XmlEncodingAssistantTest, PublicImportAssistantCancelsChildEditor)
{
    result.driver_ran = false;
    result.dialog_destroy_count = 0;
    g_weak_ref_set (&result.dialog_ref, nullptr);
    result.driver_source_id = g_idle_add (drive_cancel_then_parent_cancel, nullptr);
    gnc_xml_convert_single_file_async (nullptr, uri,
        +[](gboolean converted, gpointer data) {
            auto result = static_cast<ImportResult *>(data);
            EXPECT_FALSE (result->completed);
            result->completed = true;
            result->converted = converted;
        }, &result);
    EXPECT_FALSE (result.completed); // Product returned before any answer.
    while (!result.completed)
        g_main_context_iteration (nullptr, true);
    if (result.driver_source_id)
    {
        g_source_remove (result.driver_source_id);
        result.driver_source_id = 0;
    }
    EXPECT_FALSE (result.converted);
    EXPECT_TRUE (result.driver_ran);
    EXPECT_EQ (result.dialog_destroy_count, 1u);
    auto child_after_return = g_weak_ref_get (&result.dialog_ref);
    EXPECT_EQ (child_after_return, nullptr);
    g_clear_object (&child_after_return);
    auto error_after_return = g_weak_ref_get (&result.error_ref);
    EXPECT_EQ (error_after_return, nullptr);
    g_clear_object (&error_after_return);
    EXPECT_EQ (find_assistant (), nullptr);

}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for XML encoding assistant tests");
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects");
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto result = RUN_ALL_TESTS ();
    qof_close ();
    return result;
}
