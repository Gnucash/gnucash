/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-gtk-utils.h"

static gint
text_width (GtkWidget *widget, const gchar *text)
{
    PangoLayout *layout = gtk_widget_create_pango_layout (widget, text);
    gint width = 0;

    pango_layout_get_pixel_size (layout, &width, NULL);
    g_object_unref (layout);
    return width;
}

static void
test_string_list_reserves_widest_text (void)
{
    const gchar *initial_items[] = { "I", "A visibly wider drop-down value", NULL };
    const gchar *longer_item = "An even wider drop-down value added after construction";
    GtkStringList *model = gtk_string_list_new (initial_items);
    GtkDropDown *drop_down = gnc_gtk_drop_down_new (G_LIST_MODEL (model), NULL);
    gint first_width;
    gint second_width;
    gint updated_width;
    gint ignored_height;

    g_object_ref_sink (drop_down);
    g_assert_cmpint (text_width (GTK_WIDGET (drop_down), longer_item), >,
                     text_width (GTK_WIDGET (drop_down), initial_items[1]));

    gtk_widget_get_size_request (GTK_WIDGET (drop_down), &first_width,
                                 &ignored_height);
    g_assert_cmpint (first_width, >=,
                     text_width (GTK_WIDGET (drop_down), initial_items[1]));

    gtk_drop_down_set_selected (drop_down, 1);
    gtk_widget_get_size_request (GTK_WIDGET (drop_down), &second_width,
                                 &ignored_height);
    g_assert_cmpint (second_width, ==, first_width);

    gtk_string_list_append (model, longer_item);
    gtk_widget_get_size_request (GTK_WIDGET (drop_down), &updated_width,
                                 &ignored_height);
    g_assert_cmpint (updated_width, >, first_width);
    g_assert_cmpint (updated_width, >=,
                     text_width (GTK_WIDGET (drop_down), longer_item));

    g_object_unref (drop_down);
}

static void
test_non_string_model_is_unchanged (void)
{
    GListStore *model = g_list_store_new (G_TYPE_OBJECT);
    GObject *item = g_object_new (G_TYPE_OBJECT, NULL);
    GtkDropDown *drop_down;
    gint width;
    gint height;

    g_list_store_append (model, item);
    drop_down = GTK_DROP_DOWN (gtk_drop_down_new (G_LIST_MODEL (model), NULL));
    g_object_ref_sink (drop_down);
    gnc_gtk_drop_down_normalize_width (drop_down);
    gtk_widget_get_size_request (GTK_WIDGET (drop_down), &width, &height);
    g_assert_cmpint (width, ==, -1);

    g_object_unref (drop_down);
    g_object_unref (item);
}

static void
test_original_request_survives_model_changes (void)
{
    const gchar *items[] = { "A wider initial value", NULL };
    const gchar *replacement_items[] = { "short", NULL };
    GtkStringList *model = gtk_string_list_new (items);
    GtkDropDown *drop_down = GTK_DROP_DOWN (gtk_drop_down_new (G_LIST_MODEL (model), NULL));
    GtkStringList *empty_model = gtk_string_list_new (NULL);
    GtkStringList *replacement_model = gtk_string_list_new (replacement_items);
    gint width;
    gint height;

    g_object_ref_sink (drop_down);
    gtk_widget_set_size_request (GTK_WIDGET (drop_down), 300, 31);
    gnc_gtk_drop_down_normalize_width (drop_down);
    gtk_widget_get_size_request (GTK_WIDGET (drop_down), &width, &height);
    g_assert_cmpint (width, >=, 300);
    g_assert_cmpint (height, ==, 31);

    gtk_drop_down_set_model (drop_down, G_LIST_MODEL (empty_model));
    gtk_widget_get_size_request (GTK_WIDGET (drop_down), &width, &height);
    g_assert_cmpint (width, ==, 300);
    g_assert_cmpint (height, ==, 31);

    gtk_drop_down_set_model (drop_down, G_LIST_MODEL (replacement_model));
    gtk_widget_get_size_request (GTK_WIDGET (drop_down), &width, &height);
    g_assert_cmpint (width, >=, 300);
    g_assert_cmpint (height, ==, 31);

    g_object_unref (drop_down);
    g_object_unref (replacement_model);
    g_object_unref (empty_model);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gtk_init ();

    g_test_add_func ("/gnome-utils/drop-down-width/string-list",
                     test_string_list_reserves_widest_text);
    g_test_add_func ("/gnome-utils/drop-down-width/non-string-model",
                     test_non_string_model_is_unchanged);
    g_test_add_func ("/gnome-utils/drop-down-width/original-request",
                     test_original_request_survives_model_changes);
    return g_test_run ();
}
