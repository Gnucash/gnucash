/********************************************************************\
 * test-gnc-accelerators.c -- GTK4 accelerator override tests       *
 * Copyright (C) 2026 GnuCash Developers                            *
 *                                                                  *
 * This program is free software: you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#include <config.h>

#include <glib.h>
#include <glib/gstdio.h>

#include "gnc-gtk-utils.h"

static void
test_legacy_accelerator_map (void)
{
    const gchar *contents =
        "; Legacy accelerator map\n"
        "(gtk_accel_path \"<Actions>/gnc-plugin-basic-commands-actions/FileOpenAction\" \"<Control><Shift>o\")\n"
        "(gtk_accel_path \"<Actions>/gnc-plugin-basic-commands-actions/FileSaveAction\" \"\")\n"
        "(gtk_accel_path \"invalid\" \"<Control>i\")\n";
    const gchar *accelerator = NULL;
    gchar *filename = NULL;
    GError *error = NULL;
    gint fd;

    fd = g_file_open_tmp ("gnc-accelerator-map-XXXXXX", &filename, &error);
    g_assert_no_error (error);
    g_assert_cmpint (fd, >=, 0);
    g_assert_true (g_close (fd, &error));
    g_assert_no_error (error);
    g_assert_true (g_file_set_contents (filename, contents, -1, &error));
    g_assert_no_error (error);

    gnc_accelerator_overrides_load_legacy_map (filename);

    g_assert_true (gnc_accelerator_overrides_lookup (
                       "gnc-plugin-basic-commands-actions.FileOpenAction",
                       &accelerator));
    g_assert_cmpstr (accelerator, ==, "<Control><Shift>o");
    g_assert_true (gnc_accelerator_overrides_lookup (
                       "gnc-plugin-basic-commands-actions.FileSaveAction",
                       &accelerator));
    g_assert_cmpstr (accelerator, ==, "");
    g_assert_false (gnc_accelerator_overrides_lookup (
                        "gnc-plugin-basic-commands-actions.FileNewAction",
                        &accelerator));

    gnc_accelerator_overrides_clear ();
    g_assert_cmpint (g_remove (filename), ==, 0);
    g_free (filename);
}

static void
test_texture_from_pixbuf (void)
{
    GdkPixbuf *pixbuf;
    GdkTexture *texture;

    pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, 1, 1);
    g_assert_nonnull (pixbuf);
    gdk_pixbuf_fill (pixbuf, 0xff0000ff);

    texture = gnc_texture_new_from_pixbuf (pixbuf);
    g_assert_nonnull (texture);
    g_assert_cmpint (gdk_texture_get_width (texture), ==, 1);
    g_assert_cmpint (gdk_texture_get_height (texture), ==, 1);

    g_object_unref (texture);
    g_object_unref (pixbuf);
}

static void
test_primary_accelerator (void)
{
    GtkShortcutTrigger *trigger = gnc_accelerator_trigger_parse ("<Primary><Shift>s");
    GdkModifierType modifiers;

    g_assert_true (GTK_IS_KEYVAL_TRIGGER (trigger));
    modifiers = gtk_keyval_trigger_get_modifiers (GTK_KEYVAL_TRIGGER (trigger));
#ifdef MAC_INTEGRATION
    g_assert_cmpint (modifiers, ==, GDK_META_MASK | GDK_SHIFT_MASK);
#else
    g_assert_cmpint (modifiers, ==, GDK_CONTROL_MASK | GDK_SHIFT_MASK);
#endif
    g_object_unref (trigger);

    /* An explicitly requested Control modifier must never become Command. */
    trigger = gnc_accelerator_trigger_parse ("<Control><Meta>v");
    g_assert_true (GTK_IS_KEYVAL_TRIGGER (trigger));
    g_assert_cmpint (gtk_keyval_trigger_get_modifiers (GTK_KEYVAL_TRIGGER (trigger)),
                     ==, GDK_CONTROL_MASK | GDK_META_MASK);
    g_object_unref (trigger);
}

static void
test_menu_accelerator_label (void)
{
    GMenu *root = g_menu_new ();
    GMenu *submenu = g_menu_new ();
    GMenuItem *item = g_menu_item_new ("Save", "app.save");
    gchar *displayed = NULL;
    gchar *original = NULL;
    gchar *filename = NULL;
    GError *error = NULL;
    gint fd;

    g_menu_item_set_attribute (item, "accel", "s", "<Primary>s");
    g_menu_append_item (submenu, item);
    g_menu_append_submenu (root, "File", G_MENU_MODEL (submenu));
    g_object_unref (item);

    gnc_menu_model_apply_accelerators (G_MENU_MODEL (root));
    g_menu_model_get_item_attribute (G_MENU_MODEL (submenu), 0, "accel",
                                     "s", &displayed);
#ifdef MAC_INTEGRATION
    g_assert_cmpstr (displayed, ==, "<Meta>s");
    g_menu_model_get_item_attribute (G_MENU_MODEL (submenu), 0,
                                     "gnc-original-accel", "s", &original);
    g_assert_cmpstr (original, ==, "<Primary>s");
#else
    g_assert_cmpstr (displayed, ==, "<Primary>s");
#endif
    g_clear_pointer (&displayed, g_free);
    g_clear_pointer (&original, g_free);

    fd = g_file_open_tmp ("gnc-accelerator-label-XXXXXX", &filename, &error);
    g_assert_no_error (error);
    g_assert_cmpint (fd, >=, 0);
    g_assert_true (g_close (fd, &error));
    g_assert_no_error (error);
    g_assert_true (g_file_set_contents
                   (filename,
                    "(gtk_accel_path \"<Actions>/app/save\" \"<Control><Meta>s\")\n",
                    -1, &error));
    g_assert_no_error (error);
    gnc_accelerator_overrides_load_legacy_map (filename);
    gnc_menu_model_apply_accelerators (G_MENU_MODEL (root));
    g_menu_model_get_item_attribute (G_MENU_MODEL (submenu), 0, "accel",
                                     "s", &displayed);
    g_assert_cmpstr (displayed, ==, "<Control><Meta>s");
    g_clear_pointer (&displayed, g_free);

    gnc_accelerator_overrides_clear ();
    gnc_menu_model_apply_accelerators (G_MENU_MODEL (root));
    g_menu_model_get_item_attribute (G_MENU_MODEL (submenu), 0, "accel",
                                     "s", &displayed);
#ifdef MAC_INTEGRATION
    g_assert_cmpstr (displayed, ==, "<Meta>s");
#else
    g_assert_cmpstr (displayed, ==, "<Primary>s");
#endif

    g_free (displayed);
    g_assert_cmpint (g_remove (filename), ==, 0);
    g_free (filename);
    g_object_unref (submenu);
    g_object_unref (root);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/gnome-utils/accelerators/legacy-map",
                     test_legacy_accelerator_map);
    g_test_add_func ("/gnome-utils/accelerators/primary-modifier",
                     test_primary_accelerator);
    g_test_add_func ("/gnome-utils/accelerators/menu-label",
                     test_menu_accelerator_label);
    g_test_add_func ("/gnome-utils/texture/pixbuf",
                     test_texture_from_pixbuf);

    return g_test_run ();
}
