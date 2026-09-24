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

#include <glib.h>
#include <gdk/gdk.h>

#include "dialog-utils.h"

static void
test_modifier_mapping (void)
{
    GncRegisterInput input = { 0 };

    gnc_register_input_from_keyval (&input, GDK_KEY_Tab,
                                    GDK_CONTROL_MASK | GDK_ALT_MASK);

    g_assert_true (input.pressed);
    g_assert_cmpint (input.key, ==, GNC_REGISTER_KEY_TAB);
    g_assert_true (input.modifiers & GNC_REGISTER_MODIFIER_CONTROL);
    g_assert_true (input.modifiers & GNC_REGISTER_MODIFIER_ALT);
}

static void
test_unicode_mapping (void)
{
    GncRegisterInput input = { 0 };

    gnc_register_input_from_keyval (&input, GDK_KEY_m, 0);

    g_assert_true (input.pressed);
    g_assert_cmpint (input.key, ==, GNC_REGISTER_KEY_OTHER);
    g_assert_cmpuint (input.unicode_value, ==, 'm');
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/register-input/modifiers", test_modifier_mapping);
    g_test_add_func ("/register-input/unicode", test_unicode_mapping);

    return g_test_run ();
}
