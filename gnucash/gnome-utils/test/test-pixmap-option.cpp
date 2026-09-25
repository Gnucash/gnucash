/********************************************************************\
 * test-pixmap-option.cpp -- PIXMAP option preview tests            *
 * Copyright 2026 copystring                                       *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice: +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Boston, MA  02110-1301,  USA  *
 *                                                                  *
\********************************************************************/

#include <config.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <glib/gstdio.h>
#include <gtk/gtk.h>

#include <string>
#include <type_traits>

#include "gnc-option.hpp"
#include "gnc-option-gtk-ui.hpp"

static_assert (!std::is_copy_constructible_v<GncOption>);
static_assert (!std::is_copy_assignable_v<GncOption>);
static_assert (std::is_nothrow_destructible_v<GncOption>);
static_assert (std::is_nothrow_move_constructible_v<GncOption>);
static_assert (std::is_nothrow_move_assignable_v<GncOption>);

struct PixmapFiles
{
    gchar *directory;
    gchar *square;
    gchar *wide;
    gchar *tall;
    gchar *small;
    gchar *corrupt;
};

static gchar *
write_test_png (const gchar *directory, const gchar *name, gint width, gint height)
{
    auto filename = g_build_filename (directory, name, NULL);
    auto pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, width, height);
    GError *error = NULL;

    g_assert_nonnull (pixbuf);
    gdk_pixbuf_fill (pixbuf, 0x336699ff);
    g_assert_true (gdk_pixbuf_save (pixbuf, filename, "png", &error, NULL));
    g_assert_no_error (error);
    g_object_unref (pixbuf);
    return filename;
}

static PixmapFiles
pixmap_files_new (void)
{
    PixmapFiles files{};
    GError *error = NULL;

    files.directory = g_dir_make_tmp ("gnc-pixmap-option-XXXXXX", &error);
    g_assert_no_error (error);
    g_assert_nonnull (files.directory);
    files.square = write_test_png (files.directory, "square.png", 1024, 1024);
    files.wide = write_test_png (files.directory, "wide.png", 1024, 256);
    files.tall = write_test_png (files.directory, "tall.png", 256, 1024);
    files.small = write_test_png (files.directory, "small.png", 16, 8);
    files.corrupt = g_build_filename (files.directory, "corrupt.png", NULL);
    g_assert_true (g_file_set_contents (files.corrupt, "not a PNG", -1, &error));
    g_assert_no_error (error);
    return files;
}

static void
pixmap_files_free (PixmapFiles *files)
{
    g_remove (files->square);
    g_remove (files->wide);
    g_remove (files->tall);
    g_remove (files->small);
    g_remove (files->corrupt);
    g_rmdir (files->directory);
    g_free (files->square);
    g_free (files->wide);
    g_free (files->tall);
    g_free (files->small);
    g_free (files->corrupt);
    g_free (files->directory);
}

static GtkPicture *
create_pixmap_option (GncOption& option)
{
    auto grid = gtk_grid_new ();
    GncOptionGtkUIItem *item;
    GtkWidget *root;
    GtkWidget *picture;

    g_object_ref_sink (grid);
    GncOptionUIFactory::create (option, GTK_GRID (grid), 0);
    item = dynamic_cast<GncOptionGtkUIItem *> (option.get_ui_item ());
    g_assert_nonnull (item);
    root = item->get_widget ();
    picture = gtk_widget_get_first_child (root);
    g_assert_true (GTK_IS_PICTURE (picture));
    g_object_unref (grid);
    return GTK_PICTURE (picture);
}

static GtkWidget *
pixmap_option_root (GncOption& option)
{
    auto item = dynamic_cast<GncOptionGtkUIItem *> (option.get_ui_item ());

    g_assert_nonnull (item);
    return item->get_widget ();
}

static void
drain_main_context (void)
{
    while (g_main_context_pending (NULL))
        g_main_context_iteration (NULL, FALSE);
}

struct FrameWait
{
    GMainLoop *loop;
    gboolean frame_seen;
    gboolean timed_out;
    guint tick_id;
    GdkFrameClock *clock;
    gulong after_paint_id;
};

static void
frame_wait_after_paint_cb (GdkFrameClock *clock, gpointer user_data)
{
    auto wait = static_cast<FrameWait *> (user_data);

    wait->frame_seen = TRUE;
    if (g_main_loop_is_running (wait->loop))
        g_main_loop_quit (wait->loop);
    (void)clock;
}

static gboolean
frame_wait_tick_cb (GtkWidget *widget, GdkFrameClock *clock, gpointer user_data)
{
    auto wait = static_cast<FrameWait *> (user_data);

    wait->tick_id = 0;
    wait->clock = GDK_FRAME_CLOCK (g_object_ref (clock));
    wait->after_paint_id = g_signal_connect (clock, "after-paint",
                                              G_CALLBACK (frame_wait_after_paint_cb), wait);
    gdk_frame_clock_request_phase (clock, GDK_FRAME_CLOCK_PHASE_AFTER_PAINT);
    (void)widget;
    return G_SOURCE_REMOVE;
}

static gboolean
frame_wait_timeout_cb (gpointer user_data)
{
    auto wait = static_cast<FrameWait *> (user_data);

    wait->timed_out = TRUE;
    if (g_main_loop_is_running (wait->loop))
        g_main_loop_quit (wait->loop);
    return G_SOURCE_REMOVE;
}

static void
present_and_wait_for_frame (GtkWindow *window)
{
    auto loop = g_main_loop_new (NULL, FALSE);
    FrameWait wait{loop, FALSE, FALSE, 0, NULL, 0};
    /* Register before presenting so the presentation's first after-paint is observable. */
    wait.tick_id = gtk_widget_add_tick_callback (GTK_WIDGET (window), frame_wait_tick_cb,
                                                  &wait, NULL);

    gtk_window_present (window);
    guint timeout_id = 0;
    if (!wait.frame_seen)
    {
        timeout_id = g_timeout_add (1000, frame_wait_timeout_cb, &wait);
        g_main_loop_run (loop);
    }
    if (wait.tick_id)
        gtk_widget_remove_tick_callback (GTK_WIDGET (window), wait.tick_id);
    if (timeout_id && !wait.timed_out)
        g_source_remove (timeout_id);
    if (wait.after_paint_id)
        g_signal_handler_disconnect (wait.clock, wait.after_paint_id);
    g_clear_object (&wait.clock);
    g_main_loop_unref (loop);
    if (wait.timed_out)
        g_test_message ("Pixmap preview frame timeout: window mapped=%d realized=%d size=%dx%d",
                        gtk_widget_get_mapped (GTK_WIDGET (window)),
                        gtk_widget_get_realized (GTK_WIDGET (window)),
                        gtk_widget_get_width (GTK_WIDGET (window)),
                        gtk_widget_get_height (GTK_WIDGET (window)));
    g_assert_true (wait.frame_seen);
    g_assert_false (wait.timed_out);
}

static void
assert_preview_size (GtkPicture *picture, gint width, gint height)
{
    auto paintable = gtk_picture_get_paintable (picture);
    gint request_width;
    gint request_height;

    g_assert_nonnull (paintable);
    g_assert_cmpint (gdk_paintable_get_intrinsic_width (paintable), ==, width);
    g_assert_cmpint (gdk_paintable_get_intrinsic_height (paintable), ==, height);
    gtk_widget_get_size_request (GTK_WIDGET (picture), &request_width, &request_height);
    g_assert_cmpint (request_width, ==, 128);
    g_assert_cmpint (request_height, ==, 128);
}

static void
assert_preview_measure_and_allocation (GncOption& option, GtkPicture *picture)
{
    auto window = GTK_WINDOW (gtk_window_new ());
    auto root = pixmap_option_root (option);
    gint minimum;
    gint natural;

    gtk_widget_measure (GTK_WIDGET (picture), GTK_ORIENTATION_HORIZONTAL, -1,
                        &minimum, &natural, NULL, NULL);
    g_assert_cmpint (minimum, <=, 128);
    g_assert_cmpint (natural, <=, 128);
    gtk_widget_measure (GTK_WIDGET (picture), GTK_ORIENTATION_VERTICAL, -1,
                        &minimum, &natural, NULL, NULL);
    g_assert_cmpint (minimum, <=, 128);
    g_assert_cmpint (natural, <=, 128);

    g_object_ref_sink (window);
    gtk_window_set_default_size (window, 900, 300);
    gtk_window_set_child (window, root);
    present_and_wait_for_frame (window);
    g_assert_true (gtk_widget_get_mapped (GTK_WIDGET (window)));
    g_assert_true (gtk_widget_get_mapped (GTK_WIDGET (picture)));
    g_assert_cmpint (gtk_widget_get_width (GTK_WIDGET (picture)), >, 0);
    g_assert_cmpint (gtk_widget_get_height (GTK_WIDGET (picture)), >, 0);
    g_assert_cmpint (gtk_widget_get_width (GTK_WIDGET (picture)), <=, 128);
    g_assert_cmpint (gtk_widget_get_height (GTK_WIDGET (picture)), <=, 128);
    gtk_window_destroy (window);
    g_object_unref (window);
    drain_main_context ();
}

static void
test_pixmap_preview_is_bounded_for_initial_value_and_changes (void)
{
    PixmapFiles files = pixmap_files_new ();
    GncOption option{"test", "Pixmap", "pixmap", "", std::string{files.square},
                      GncOptionUIType::PIXMAP};
    auto picture = create_pixmap_option (option);

    assert_preview_size (picture, 128, 128);
    assert_preview_measure_and_allocation (option, picture);

    option.set_value (std::string{files.wide});
    option.set_ui_item_from_option ();
    assert_preview_size (picture, 128, 32);

    option.set_value (std::string{files.tall});
    option.set_ui_item_from_option ();
    assert_preview_size (picture, 32, 128);

    option.set_value (std::string{files.small});
    option.set_ui_item_from_option ();
    auto paintable = gtk_picture_get_paintable (picture);
    g_assert_nonnull (paintable);
    g_assert_cmpint (gdk_paintable_get_intrinsic_width (paintable), <=, 128);
    g_assert_cmpint (gdk_paintable_get_intrinsic_height (paintable), <=, 128);
    g_assert_cmpint (gdk_paintable_get_intrinsic_width (paintable), ==,
                     2 * gdk_paintable_get_intrinsic_height (paintable));

    pixmap_files_free (&files);
}

static GtkWidget *
create_retained_pixmap_root (const gchar *path)
{
    GtkWidget *root;

    {
        GncOption option{"test", "Pixmap", "pixmap", "", std::string{path},
                          GncOptionUIType::PIXMAP};

        create_pixmap_option (option);
        root = pixmap_option_root (option);
        g_object_ref (root);
    }
    return root;
}

static GtkButton *
pixmap_option_button (GtkWidget *root, guint index)
{
    auto child = gtk_widget_get_first_child (root);

    for (guint current = 0; child && current < index;
         child = gtk_widget_get_next_sibling (child), current++)
        ;
    g_assert_true (GTK_IS_BUTTON (child));
    return GTK_BUTTON (child);
}

static void
assert_invalidated_pixmap_callbacks (GtkWidget *root)
{
    auto picture = GTK_PICTURE (gtk_widget_get_first_child (root));
    auto paintable = gtk_picture_get_paintable (picture);
    auto choose = pixmap_option_button (root, 2);
    auto clear = pixmap_option_button (root, 3);

    g_assert_nonnull (paintable);
    g_object_ref (paintable);
    g_assert_null (g_object_get_data (G_OBJECT (root), "gnc-pixmap-option"));
    g_signal_emit_by_name (clear, "clicked");
    g_assert_true (gtk_picture_get_paintable (picture) == paintable);
    g_signal_emit_by_name (choose, "clicked");
    g_assert_true (gtk_picture_get_paintable (picture) == paintable);
    g_object_unref (paintable);
}

static void
test_pixmap_callbacks_ignore_destroyed_option (void)
{
    PixmapFiles files = pixmap_files_new ();
    auto root = create_retained_pixmap_root (files.square);

    assert_invalidated_pixmap_callbacks (root);
    g_object_unref (root);
    pixmap_files_free (&files);
}

static void
test_pixmap_callbacks_ignore_cleared_ui_item (void)
{
    PixmapFiles files = pixmap_files_new ();
    GncOption option{"test", "Pixmap", "pixmap", "", std::string{files.square},
                      GncOptionUIType::PIXMAP};

    create_pixmap_option (option);
    auto root = pixmap_option_root (option);
    g_object_ref (root);
    option.get_ui_item ()->clear_ui_item ();
    assert_invalidated_pixmap_callbacks (root);
    g_object_unref (root);
    pixmap_files_free (&files);
}

static void
test_pixmap_preview_reset_and_invalid_values_clear_only_the_preview (void)
{
    PixmapFiles files = pixmap_files_new ();
    auto missing = g_build_filename (files.directory, "missing.png", NULL);
    GncOption option{"test", "Pixmap", "pixmap", "", std::string{files.square},
                      GncOptionUIType::PIXMAP};
    auto picture = create_pixmap_option (option);

    option.set_value (std::string{files.wide});
    option.set_ui_item_from_option ();
    assert_preview_size (picture, 128, 32);

    option.reset_default_value ();
    option.set_ui_item_from_option ();
    auto reset_value = option.get_value<std::string> ();
    g_assert_cmpstr (reset_value.c_str (), ==, files.square);
    assert_preview_size (picture, 128, 128);

    const gchar *invalid_paths[] = { "", missing, files.corrupt };

    for (guint index = 0; index < G_N_ELEMENTS (invalid_paths); index++)
    {
        const gchar *path = invalid_paths[index];

        if (*path)
            g_test_expect_message ("gnc.gui", G_LOG_LEVEL_CRITICAL,
                                   "*Unable to load image preview*");
        option.set_value (std::string{path});
        option.set_ui_item_from_option ();
        if (*path)
            g_test_assert_expected_messages ();
        auto value = option.get_value<std::string> ();
        g_assert_cmpstr (value.c_str (), ==, path);
        g_assert_null (gtk_picture_get_paintable (picture));
    }

    g_free (missing);
    pixmap_files_free (&files);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gtk_init ();

    g_test_add_func ("/gnome-utils/pixmap-option/bounded-preview",
                     test_pixmap_preview_is_bounded_for_initial_value_and_changes);
    g_test_add_func ("/gnome-utils/pixmap-option/reset-and-invalid-values",
                     test_pixmap_preview_reset_and_invalid_values_clear_only_the_preview);
    g_test_add_func ("/gnome-utils/pixmap-option/destroyed-option-callbacks",
                     test_pixmap_callbacks_ignore_destroyed_option);
    g_test_add_func ("/gnome-utils/pixmap-option/cleared-ui-item-callbacks",
                     test_pixmap_callbacks_ignore_cleared_ui_item);
    return g_test_run ();
}
