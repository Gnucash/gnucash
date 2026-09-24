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

#include <gio/gio.h>
#include <glib/gstdio.h>

typedef struct
{
    GApplicationCommandLine parent_instance;
} TestRemoteCommandLine;

typedef struct
{
    GApplicationCommandLineClass parent_class;
} TestRemoteCommandLineClass;

GType test_remote_command_line_get_type (void);

G_DEFINE_TYPE (TestRemoteCommandLine, test_remote_command_line,
               G_TYPE_APPLICATION_COMMAND_LINE)

static void
test_remote_command_line_class_init (TestRemoteCommandLineClass *klass)
{
    (void)klass;
}

static void
test_remote_command_line_init (TestRemoteCommandLine *command_line)
{
    (void)command_line;
}

static void
test_relative_file_uses_invoker_cwd (void)
{
    const gchar *arguments[] = { "gnucash", "relative.gnucash", NULL };
    GVariantBuilder platform_data;
    GError *error = NULL;
    gchar *invoker_dir = g_dir_make_tmp ("gnucash-invoker-XXXXXX", &error);
    g_assert_no_error (error);
    gchar *primary_dir = g_dir_make_tmp ("gnucash-primary-XXXXXX", &error);
    g_assert_no_error (error);
    gchar *original_dir = g_get_current_dir ();

    g_assert_cmpint (g_chdir (primary_dir), ==, 0);

    g_variant_builder_init (&platform_data, G_VARIANT_TYPE_VARDICT);
    g_variant_builder_add (&platform_data, "{sv}", "cwd",
                           g_variant_new_bytestring (invoker_dir));

    GApplicationCommandLine *command_line = g_object_new (
        test_remote_command_line_get_type (),
        "arguments", g_variant_new_bytestring_array (arguments, -1),
        "platform-data", g_variant_builder_end (&platform_data),
        NULL);

    g_assert_true (g_application_command_line_get_is_remote (command_line));
    g_assert_cmpstr (g_application_command_line_get_cwd (command_line), ==,
                     invoker_dir);

    GFile *actual = g_application_command_line_create_file_for_arg (
        command_line, arguments[1]);
    gchar *expected_path = g_build_filename (invoker_dir, arguments[1], NULL);
    GFile *expected = g_file_new_for_path (expected_path);

    g_assert_true (g_file_equal (actual, expected));

    g_object_unref (expected);
    g_free (expected_path);
    g_object_unref (actual);
    g_object_unref (command_line);

    g_assert_cmpint (g_chdir (original_dir), ==, 0);
    g_assert_cmpint (g_rmdir (primary_dir), ==, 0);
    g_assert_cmpint (g_rmdir (invoker_dir), ==, 0);
    g_free (original_dir);
    g_free (primary_dir);
    g_free (invoker_dir);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/gnucash/gapplication/relative-file-invoker-cwd",
                     test_relative_file_uses_invoker_cwd);
    return g_test_run ();
}
