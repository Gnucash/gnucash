/********************************************************************
 * testmain.c: GLib g_test test execution file.			    *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>		    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/


#include <config.h>
#include <string.h>
#include <glib.h>
#include <gnc-glib-utils.h>
#include <unittest-support.h>

static void
test_gnc_utf8_strip_invalid_and_controls (gconstpointer data)
{
    gchar *str = g_strdup (data);
    const gchar *controls = "\b\f\n\r\t\v\x01\x02\x03\x04\x05\x06\x07"
        "\x08\x09\xa\xb\xc\xd\xe\xf\x10\x11\x12\x13\x14\x15\x16"
        "\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f";
    char *msg1 = g_strdup_printf ("Invalid utf8 string: %s",
                                  (const gchar*)data);
    const GLogLevelFlags level = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    TestErrorStruct check = {level, NULL, msg1, 0};

    guint handler = g_log_set_handler (NULL, level,
                                       (GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler((GTestLogFatalFunc)test_checked_handler,
                                 &check);

    gnc_utf8_strip_invalid_and_controls (str);
    g_assert (g_utf8_validate(str, -1, NULL) == TRUE);
    g_assert (strpbrk(str, controls) == NULL);
    g_assert (g_utf8_strlen(str, -1) > 0);
    g_log_remove_handler (NULL, handler);
    g_free (str);
    g_free (msg1);
}


int
main (int argc, char *argv[])
{
    const gchar *invalid_utf8 = "Η γρήγορη καφέ αλεπού πήδηξε πάνω από την \xb2\xf3ργή σκύλο.";
    const gchar *controls = "Η γρήγορη καφέ αλεπού\bπήδηξε\nπάνω από\tτην αργή σκύλο.";
    g_test_init (&argc, &argv, NULL); // initialize test program
    g_test_add_data_func ("/core-utils/gnc_utf8_strip_invalid_and_controls invalid utf8", (gconstpointer)invalid_utf8, test_gnc_utf8_strip_invalid_and_controls);
    g_test_add_data_func ("/core-utils/gnc_utf8_strip_invalid_and_controls control chars", (gconstpointer)controls, test_gnc_utf8_strip_invalid_and_controls);

    return g_test_run();
}
