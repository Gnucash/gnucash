/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
#include <glib/gstdio.h>

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
GtkWidget *assistant_under_test;
gboolean display_available;
guint driver_source_id;
gboolean driver_ran;
guint encoding_dialog_destroy_count;
GWeakRef encoding_dialog_ref;
GWeakRef encoding_error_ref;

struct EncodingPath
{
    const char *encoding;
    GtkTreePath *path;
};

gboolean
find_encoding_path (GtkTreeModel *model, GtkTreePath *path,
                    GtkTreeIter *iter, gpointer user_data)
{
    auto wanted = static_cast<EncodingPath *> (user_data);
    gpointer quark_ptr = nullptr;
    g_assert_cmpuint (gtk_tree_model_get_column_type (model, 1), ==,
                      G_TYPE_POINTER);
    gtk_tree_model_get (model, iter, 1, &quark_ptr, -1);
    auto encoding = g_quark_to_string (GPOINTER_TO_UINT (quark_ptr));
    if (g_strcmp0 (encoding, wanted->encoding) != 0)
        return FALSE;
    wanted->path = gtk_tree_path_copy (path);
    return TRUE;
}

GtkWidget *
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

GtkWidget *
find_assistant ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        if (GTK_IS_ASSISTANT (node->data))
        {
            g_assert_null (result);
            result = GTK_WIDGET (node->data);
        }
    }
    g_list_free (windows);
    return result;
}

GtkWidget *
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
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

void
track_dialog_destruction (GtkWidget *dialog)
{
    g_weak_ref_set (&encoding_dialog_ref, G_OBJECT (dialog));
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (+[] (GtkWidget *, gpointer data)
                      {
                          ++*static_cast<guint *> (data);
                      }), &encoding_dialog_destroy_count);
}

guint
row_count (GtkWidget *view)
{
    return gtk_tree_model_iter_n_children (
        gtk_tree_view_get_model (GTK_TREE_VIEW (view)), nullptr);
}

void
remove_latin1_encoding (GtkWidget *dialog)
{
    auto view = find_named_child (dialog, "selected_encs_view");
    auto remove = find_named_child (dialog, "remove_enc_button");
    g_assert_nonnull (view);
    g_assert_nonnull (remove);

    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    EncodingPath wanted {"ISO-8859-1", nullptr};
    gtk_tree_model_foreach (model, find_encoding_path, &wanted);
    g_assert_nonnull (wanted.path);
    gtk_tree_selection_select_path (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), wanted.path);
    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), wanted.path, nullptr, FALSE);
    gtk_tree_path_free (wanted.path);
    gtk_button_clicked (GTK_BUTTON (remove));
}

void
add_latin1_encoding (GtkWidget *dialog)
{
    auto view = find_named_child (dialog, "available_encs_view");
    auto add = find_named_child (dialog, "add_enc_button");
    g_assert_nonnull (view);
    g_assert_nonnull (add);
    EncodingPath wanted {"ISO-8859-1", nullptr};
    gtk_tree_model_foreach (gtk_tree_view_get_model (GTK_TREE_VIEW (view)),
                            find_encoding_path, &wanted);
    g_assert_nonnull (wanted.path);
    gtk_tree_view_expand_to_path (GTK_TREE_VIEW (view), wanted.path);
    gtk_tree_selection_select_path (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), wanted.path);
    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), wanted.path, nullptr, FALSE);
    g_assert_true (gtk_tree_selection_path_is_selected (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), wanted.path));
    gtk_tree_path_free (wanted.path);
    gtk_button_clicked (GTK_BUTTON (add));
}

void
check_rejected_encoding (GtkWidget *dialog, const char *encoding)
{
    auto selected = find_named_child (dialog, "selected_encs_view");
    auto entry = find_named_child (dialog, "custom_enc_entry");
    g_assert_true (GTK_IS_ENTRY (entry));
    auto count = row_count (selected);
    gtk_entry_set_text (GTK_ENTRY (entry), encoding);
    auto add = find_named_child (dialog, "add_custom_enc_button");
    g_assert_true (GTK_IS_BUTTON (add));
    gtk_button_clicked (GTK_BUTTON (add));
    auto error = find_encoding_dialog (dialog);
    g_assert_true (GTK_IS_MESSAGE_DIALOG (error));
    g_assert_cmpuint (row_count (selected), ==, count);
    gtk_dialog_response (GTK_DIALOG (error), GTK_RESPONSE_CLOSE);
    g_assert_null (find_encoding_dialog (dialog));
}

gboolean
drive_cancel_then_parent_cancel (gpointer)
{
    driver_source_id = 0;
    driver_ran = TRUE;
    assistant_under_test = find_assistant ();
    g_assert_nonnull (assistant_under_test);
    gtk_assistant_set_current_page (GTK_ASSISTANT (assistant_under_test), 1);

    auto edit = find_named_child (assistant_under_test, "edit_encs_button");
    g_assert_nonnull (edit);
    gtk_button_clicked (GTK_BUTTON (edit));
    auto dialog = find_encoding_dialog (assistant_under_test);
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto selected = find_named_child (dialog, "selected_encs_view");
    g_assert_nonnull (selected);
    auto original_count = row_count (selected);
    check_rejected_encoding (dialog, "ISO-8859-1");
    check_rejected_encoding (dialog, "gnc-invalid-test-encoding");
    remove_latin1_encoding (dialog);
    g_assert_cmpuint (row_count (selected), ==, original_count - 1);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    g_assert_null (find_encoding_dialog (assistant_under_test));

    gtk_button_clicked (GTK_BUTTON (edit));
    dialog = find_encoding_dialog (assistant_under_test);
    g_assert_nonnull (dialog);
    selected = find_named_child (dialog, "selected_encs_view");
    g_assert_cmpuint (row_count (selected), ==, original_count);
    remove_latin1_encoding (dialog);
    g_assert_cmpuint (row_count (selected), ==, original_count - 1);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    gtk_button_clicked (GTK_BUTTON (edit));
    dialog = find_encoding_dialog (assistant_under_test);
    g_assert_nonnull (dialog);
    track_dialog_destruction (dialog);
    selected = find_named_child (dialog, "selected_encs_view");
    g_assert_cmpuint (row_count (selected), ==, original_count - 1);
    add_latin1_encoding (dialog);
    g_assert_cmpuint (row_count (selected), ==, original_count);
    auto entry = find_named_child (dialog, "custom_enc_entry");
    gtk_entry_set_text (GTK_ENTRY (entry), "gnc-invalid-test-encoding");
    auto add = find_named_child (dialog, "add_custom_enc_button");
    g_assert_true (GTK_IS_BUTTON (add));
    gtk_button_clicked (GTK_BUTTON (add));
    auto error = find_encoding_dialog (dialog);
    g_assert_true (GTK_IS_MESSAGE_DIALOG (error));
    g_weak_ref_set (&encoding_error_ref, G_OBJECT (error));

    /* Closing the parent while the editor is open must dispose the child and
       roll back its unaccepted working list before importer state is freed. */
    g_signal_emit_by_name (assistant_under_test, "cancel");
    return G_SOURCE_REMOVE;
}

void
test_public_import_assistant ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    gchar *filename = nullptr;
    gint fd = g_file_open_tmp ("gnc-xml-encoding-XXXXXX", &filename, nullptr);
    g_assert_cmpint (fd, >=, 0);
    g_assert_cmpint (g_close (fd, nullptr), ==, TRUE);
    g_assert_true (g_file_set_contents (filename, xml_fixture,
                                        sizeof (xml_fixture) - 1, nullptr));
    gchar *uri = g_filename_to_uri (filename, nullptr, nullptr);
    g_assert_nonnull (uri);

    driver_ran = FALSE;
    encoding_dialog_destroy_count = 0;
    g_weak_ref_set (&encoding_dialog_ref, nullptr);
    driver_source_id = g_idle_add (drive_cancel_then_parent_cancel, nullptr);
    struct Result { bool completed; gboolean converted; } result {false, FALSE};
    gnc_xml_convert_single_file_async (nullptr, uri,
        +[](gboolean converted, gpointer data) {
            auto result = static_cast<Result *>(data);
            g_assert_false (result->completed);
            result->completed = true;
            result->converted = converted;
        }, &result);
    g_assert_false (result.completed); // Product returned before any answer.
    while (!result.completed)
        g_main_context_iteration (nullptr, TRUE);
    if (driver_source_id)
    {
        g_source_remove (driver_source_id);
        driver_source_id = 0;
    }
    g_assert_false (result.converted);
    g_assert_true (driver_ran);
    g_assert_cmpuint (encoding_dialog_destroy_count, ==, 1);
    auto child_after_return = g_weak_ref_get (&encoding_dialog_ref);
    g_assert_null (child_after_return);
    g_clear_object (&child_after_return);
    auto error_after_return = g_weak_ref_get (&encoding_error_ref);
    g_assert_null (error_after_return);
    g_clear_object (&error_after_return);
    g_assert_null (find_assistant ());

    g_free (uri);
    g_remove (filename);
    g_free (filename);
}
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_set_current_session (qof_session_new (qof_book_new ()));
    g_weak_ref_init (&encoding_dialog_ref, nullptr);
    g_weak_ref_init (&encoding_error_ref, nullptr);
    g_test_add_func ("/gnome-utils/xml-encoding/public-import-assistant",
                     test_public_import_assistant);
    auto result = g_test_run ();
    g_weak_ref_clear (&encoding_dialog_ref);
    g_weak_ref_clear (&encoding_error_ref);
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
