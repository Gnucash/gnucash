/********************************************************************
 * test-gnc-register-editor-size.c -- register editor CSS tests     *
 *                                                                  *
 * Copyright (C) 2026 GnuCash Contributors                          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <config.h>

#include <gtk/gtk.h>

typedef struct
{
    GtkBox parent_instance;
} TestRegisterCursor;

typedef GtkBoxClass TestRegisterCursorClass;

GType test_register_cursor_get_type (void);

G_DEFINE_FINAL_TYPE (TestRegisterCursor, test_register_cursor, GTK_TYPE_BOX)

static void
test_register_cursor_class_init (TestRegisterCursorClass *klass)
{
    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS (klass), "gnc-id-cursor");
}

static void
test_register_cursor_init (TestRegisterCursor *cursor)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE (cursor),
                                    GTK_ORIENTATION_HORIZONTAL);
}

static void
load_gnucash_css (void)
{
    GtkCssProvider *provider = gtk_css_provider_new ();
    GdkDisplay *display = gdk_display_get_default ();

    g_assert_nonnull (display);
    gtk_css_provider_load_from_path (provider, GNC_SOURCE_CSS);
    gtk_style_context_add_provider_for_display
        (display, GTK_STYLE_PROVIDER (provider),
         GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    g_object_unref (provider);
}

static void
measure_widget (GtkWidget *widget, GtkOrientation orientation,
                int *minimum, int *natural)
{
    gtk_widget_measure (widget, orientation, -1, minimum, natural, NULL, NULL);
}

static void
test_register_editor_children_fit_one_text_line (void)
{
    GtkWidget *window = gtk_window_new ();
    GtkWidget *cursor = g_object_new (test_register_cursor_get_type (), NULL);
    GtkWidget *entry = gtk_entry_new ();
    GtkWidget *button = gtk_toggle_button_new ();
    GtkWidget *icon = gtk_image_new_from_icon_name ("pan-down-symbolic");
    PangoLayout *layout;
    int text_height;
    int entry_min_height;
    int entry_nat_height;
    int button_min_width;
    int button_nat_width;
    int button_min_height;
    int button_nat_height;

    gtk_entry_set_has_frame (GTK_ENTRY (entry), FALSE);
    gtk_button_set_child (GTK_BUTTON (button), icon);
    gtk_box_append (GTK_BOX (cursor), entry);
    gtk_box_append (GTK_BOX (cursor), button);
    gtk_window_set_child (GTK_WINDOW (window), cursor);

    gtk_widget_realize (window);

    layout = gtk_widget_create_pango_layout (entry, "8.8.2026");
    pango_layout_get_pixel_size (layout, NULL, &text_height);
    g_object_unref (layout);

    measure_widget (entry, GTK_ORIENTATION_VERTICAL,
                    &entry_min_height, &entry_nat_height);
    measure_widget (button, GTK_ORIENTATION_HORIZONTAL,
                    &button_min_width, &button_nat_width);
    measure_widget (button, GTK_ORIENTATION_VERTICAL,
                    &button_min_height, &button_nat_height);

    g_test_message ("text=%d entry=%d/%d button-width=%d/%d "
                    "button-height=%d/%d",
                    text_height, entry_min_height, entry_nat_height,
                    button_min_width, button_nat_width,
                    button_min_height, button_nat_height);

    g_assert_cmpint (entry_min_height, <=, text_height + 6);
    g_assert_cmpint (button_min_width, <=, entry_min_height);
    g_assert_cmpint (button_min_height, <=, entry_min_height);

    gtk_window_destroy (GTK_WINDOW (window));
}

int
main (int argc, char **argv)
{
    gtk_init ();
    g_test_init (&argc, &argv, NULL);
    load_gnucash_css ();
    g_test_add_func ("/register/editor/children-fit-one-text-line",
                     test_register_editor_children_fit_one_text_line);
    return g_test_run ();
}
