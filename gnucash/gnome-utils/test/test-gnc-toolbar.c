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

#include "gnc-plugin.h"

typedef struct
{
    GtkWidget *button;
    gchar *label;
    gchar *icon_name;
    gchar *action_name;
    gchar *tooltip;
} ToolbarButtonExpectation;

static const gchar *toolbar_ui_files[] =
{
    "gnc-embedded-register-window.ui",
    "gnc-main-window.ui",
    "gnc-plugin-page-account-tree.ui",
    "gnc-plugin-page-budget.ui",
    "gnc-plugin-page-invoice.ui",
    "gnc-plugin-page-owner-tree.ui",
    "gnc-plugin-page-register.ui",
    "gnc-plugin-page-report.ui",
    "gnc-plugin-page-sx-list.ui",
    NULL
};

static void
toolbar_button_expectation_free (gpointer data)
{
    ToolbarButtonExpectation *expectation = data;

    g_free (expectation->label);
    g_free (expectation->icon_name);
    g_free (expectation->action_name);
    g_free (expectation->tooltip);
    g_free (expectation);
}

static void
collect_toolbar_button_expectations (GtkWidget *toolbar, GPtrArray *expectations)
{
    GtkWidget *child;

    for (child = gtk_widget_get_first_child (toolbar);
         child != NULL;
         child = gtk_widget_get_next_sibling (child))
    {
        ToolbarButtonExpectation *expectation;
        GtkWidget *content;
        GtkWidget *image;
        GtkWidget *label;

        if (!GTK_IS_BUTTON (child))
            continue;

        content = gtk_button_get_child (GTK_BUTTON (child));
        g_assert_true (GTK_IS_BOX (content));
        g_assert_cmpint (gtk_orientable_get_orientation (GTK_ORIENTABLE (content)),
                         ==, GTK_ORIENTATION_VERTICAL);
        image = gtk_widget_get_first_child (content);
        label = gtk_widget_get_next_sibling (image);
        g_assert_true (GTK_IS_IMAGE (image));
        g_assert_true (GTK_IS_LABEL (label));
        g_assert_null (gtk_widget_get_next_sibling (label));

        expectation = g_new0 (ToolbarButtonExpectation, 1);
        expectation->button = child;
        expectation->label = g_strdup (gtk_label_get_label (GTK_LABEL (label)));
        expectation->icon_name = g_strdup (gtk_image_get_icon_name (GTK_IMAGE (image)));
        expectation->action_name =
            g_strdup (gtk_actionable_get_action_name (GTK_ACTIONABLE (child)));
        expectation->tooltip = g_strdup (gtk_widget_get_tooltip_text (child));

        g_assert_nonnull (expectation->label);
        g_assert_nonnull (expectation->icon_name);
        g_assert_nonnull (expectation->action_name);
        g_assert_nonnull (expectation->tooltip);
        g_ptr_array_add (expectations, expectation);
    }
}

static void
assert_toolbar_button_content (const ToolbarButtonExpectation *expectation)
{
    GtkWidget *content = gtk_button_get_child (GTK_BUTTON (expectation->button));
    GtkAccessibleRole expected_role = GTK_IS_TOGGLE_BUTTON (expectation->button)
                                           ? GTK_ACCESSIBLE_ROLE_TOGGLE_BUTTON
                                           : GTK_ACCESSIBLE_ROLE_BUTTON;
    GtkWidget *image;
    GtkWidget *label;
    const gchar *tooltip;

    g_assert_true (GTK_IS_BOX (content));
    g_assert_cmpint (gtk_orientable_get_orientation (GTK_ORIENTABLE (content)),
                     ==, GTK_ORIENTATION_VERTICAL);

    image = gtk_widget_get_first_child (content);
    label = gtk_widget_get_next_sibling (image);
    g_assert_true (GTK_IS_IMAGE (image));
    g_assert_true (GTK_IS_LABEL (label));
    g_assert_null (gtk_widget_get_next_sibling (label));
    g_assert_cmpstr (gtk_image_get_icon_name (GTK_IMAGE (image)), ==,
                     expectation->icon_name);
    g_assert_cmpint (gtk_image_get_pixel_size (GTK_IMAGE (image)), ==, 24);
    g_assert_cmpstr (gtk_label_get_label (GTK_LABEL (label)), ==,
                     expectation->label);
    g_assert_true (gtk_label_get_use_underline (GTK_LABEL (label)));
    g_assert_cmpstr (gtk_actionable_get_action_name (GTK_ACTIONABLE (expectation->button)),
                     ==, expectation->action_name);
    tooltip = gtk_widget_get_tooltip_text (expectation->button);
    g_assert_cmpstr (tooltip, ==, expectation->tooltip);
    g_assert_cmpint (gtk_accessible_get_accessible_role (GTK_ACCESSIBLE (expectation->button)),
                     ==, expected_role);
}

static void
assert_toolbar_normalization_is_idempotent (GtkWidget *toolbar)
{
    GPtrArray *buttons = g_ptr_array_new ();
    GPtrArray *children = g_ptr_array_new ();
    GtkWidget *child;

    for (child = gtk_widget_get_first_child (toolbar);
         child != NULL;
         child = gtk_widget_get_next_sibling (child))
    {
        if (GTK_IS_BUTTON (child))
        {
            g_ptr_array_add (buttons, child);
            g_ptr_array_add (children, gtk_button_get_child (GTK_BUTTON (child)));
        }
    }

    gnc_plugin_prepare_toolbar (toolbar);

    for (guint i = 0; i < children->len; i++)
    {
        g_assert_true (gtk_button_get_child (GTK_BUTTON (g_ptr_array_index (buttons, i))) ==
                       g_ptr_array_index (children, i));
    }
    g_ptr_array_unref (buttons);
    g_ptr_array_unref (children);
}

static void
test_toolbar_buttons_have_explicit_icon_and_label (void)
{
    const gchar *ui_source_dir = g_getenv ("GNC_UI_SOURCE_DIR");
    GPtrArray *builders = g_ptr_array_new_with_free_func (g_object_unref);
    GPtrArray *expectations =
        g_ptr_array_new_with_free_func (toolbar_button_expectation_free);
    guint toolbar_count = 0;

    g_assert_nonnull (ui_source_dir);

    for (guint i = 0; toolbar_ui_files[i] != NULL; i++)
    {
        GtkBuilder *builder = gtk_builder_new ();
        gchar *ui_file = g_build_filename (ui_source_dir, toolbar_ui_files[i], NULL);
        GError *error = NULL;
        GSList *objects;

        g_assert_true (gtk_builder_add_from_file (builder, ui_file, &error) != 0);
        g_assert_no_error (error);
        g_free (ui_file);

        for (objects = gtk_builder_get_objects (builder); objects != NULL; objects = objects->next)
        {
            GtkWidget *widget = objects->data;

            if (!GTK_IS_WIDGET (widget) || !gtk_widget_has_css_class (widget, "toolbar"))
                continue;

            toolbar_count++;
            collect_toolbar_button_expectations (widget, expectations);
            gnc_plugin_prepare_toolbar (widget);
            assert_toolbar_normalization_is_idempotent (widget);
        }
        g_slist_free (objects);
        g_ptr_array_add (builders, builder);
    }

    g_assert_cmpuint (toolbar_count, ==, 11);
    g_assert_cmpuint (expectations->len, ==, 99);

    for (guint i = 0; i < expectations->len; i++)
        assert_toolbar_button_content (g_ptr_array_index (expectations, i));

    g_ptr_array_unref (expectations);
    g_ptr_array_unref (builders);
}

static void
test_short_name_updates_explicit_toolbar_label (void)
{
    GncToolBarShortNames short_names[] =
    {
        { "ShortNameAction", "Short" },
        { NULL, NULL }
    };
    GtkWidget *toolbar = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    GtkWidget *button = gtk_button_new ();
    GtkWidget *content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    GtkWidget *image = gtk_image_new_from_icon_name ("document-save");
    GtkWidget *label = gtk_label_new_with_mnemonic ("_Long Label");

    g_object_ref_sink (toolbar);
    gtk_image_set_pixel_size (GTK_IMAGE (image), 24);
    gtk_label_set_use_underline (GTK_LABEL (label), TRUE);
    gtk_box_append (GTK_BOX (content), image);
    gtk_box_append (GTK_BOX (content), label);
    gtk_button_set_child (GTK_BUTTON (button), content);
    gtk_actionable_set_action_name (GTK_ACTIONABLE (button), "win.ShortNameAction");
    gtk_box_append (GTK_BOX (toolbar), button);

    gnc_plugin_prepare_toolbar (toolbar);
    gnc_plugin_init_short_names (toolbar, short_names);

    label = gtk_widget_get_next_sibling (gtk_widget_get_first_child (content));
    g_assert_true (GTK_IS_LABEL (label));
    g_assert_cmpstr (gtk_label_get_label (GTK_LABEL (label)), ==, "Short");
    g_assert_true (gtk_label_get_use_underline (GTK_LABEL (label)));

    g_object_unref (toolbar);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    g_test_add_func ("/gnome-utils/toolbar/explicit-icon-and-label",
                     test_toolbar_buttons_have_explicit_icon_and_label);
    g_test_add_func ("/gnome-utils/toolbar/short-name-label",
                     test_short_name_updates_explicit_toolbar_label);

    return g_test_run ();
}
