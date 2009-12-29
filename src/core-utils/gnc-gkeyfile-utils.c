/*
 * gnc-gkeyfile-utils.c -- utility functions for working
 *              with GKeyFile data structures from GLib
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup GLib
    @{ */
/** @addtogroup GKeyFile GKeyfile Utilities

    This file provides routines that help make it easier to use the
    GKeyFile functions from within Gnucash.

    @{ */
/** @file gnc-gkeyfile-utils.c
 *  @brief GKeyFile helper routines.
 *  @author Copyright (C) 2005 David Hampton <hampton@employees.org>
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include "gnc-gkeyfile-utils.h"

#ifndef HAVE_GLIB_2_12
/**********************************************************************
 *
 * The following functions are copied verbatim from the GLIB 2.12
 * source code.  If more glib 2.12 functions are included into
 * gnucash,then they should all be consolidated in a lib/glib212
 * directory.
 *
 **********************************************************************/
static gchar *
_g_utf8_make_valid (const gchar *name)
{
    GString *string;
    const gchar *remainder, *invalid;
    gint remaining_bytes, valid_bytes;

    string = NULL;
    remainder = name;
    remaining_bytes = strlen (name);

    while (remaining_bytes != 0)
    {
        if (g_utf8_validate (remainder, remaining_bytes, &invalid))
            break;
        valid_bytes = invalid - remainder;

        if (string == NULL)
            string = g_string_sized_new (remaining_bytes);

        g_string_append_len (string, remainder, valid_bytes);
        /* append U+FFFD REPLACEMENT CHARACTER */
        g_string_append (string, "\357\277\275");

        remaining_bytes -= valid_bytes + 1;
        remainder = invalid + 1;
    }

    if (string == NULL)
        return g_strdup (name);

    g_string_append (string, remainder);

    g_assert (g_utf8_validate (string->str, -1, NULL));

    return g_string_free (string, FALSE);
}

static gdouble
g_key_file_parse_value_as_double  (GKeyFile     *key_file,
                                   const gchar  *value,
                                   GError      **error)
{
    gchar *end_of_valid_d;
    gdouble double_value = 0;

    double_value = g_ascii_strtod (value, &end_of_valid_d);

    if (*end_of_valid_d != '\0' || end_of_valid_d == value)
    {
        gchar *value_utf8 = _g_utf8_make_valid (value);
        g_set_error (error, G_KEY_FILE_ERROR,
                     G_KEY_FILE_ERROR_INVALID_VALUE,
                     _("Value '%s' cannot be interpreted "
                       "as a float number."),
                     value_utf8);
        g_free (value_utf8);
    }

    return double_value;
}

gdouble
g_key_file_get_double (GKeyFile *key_file, const gchar *group_name,
                       const gchar *key, GError **error)
{
    GError *key_file_error;
    gchar *value;
    gdouble double_value;

    g_return_val_if_fail (key_file != NULL, -1);
    g_return_val_if_fail (group_name != NULL, -1);
    g_return_val_if_fail (key != NULL, -1);

    key_file_error = NULL;

    value = g_key_file_get_value (key_file, group_name, key, &key_file_error);

    if (key_file_error)
    {
        g_propagate_error (error, key_file_error);
        return 0;
    }

    double_value = g_key_file_parse_value_as_double (key_file, value,
                   &key_file_error);
    g_free (value);

    if (key_file_error)
    {
        if (g_error_matches (key_file_error,
                             G_KEY_FILE_ERROR,
                             G_KEY_FILE_ERROR_INVALID_VALUE))
        {
            g_set_error (error, G_KEY_FILE_ERROR,
                         G_KEY_FILE_ERROR_INVALID_VALUE,
                         _("Key file contains key '%s' in group '%s' "
                           "which has a value that cannot be interpreted."), key,
                         group_name);
            g_error_free (key_file_error);
        }
        else
            g_propagate_error (error, key_file_error);
    }

    return double_value;
}

void
g_key_file_set_double  (GKeyFile    *key_file,
                        const gchar *group_name,
                        const gchar *key,
                        gdouble      value)
{
    gchar result[G_ASCII_DTOSTR_BUF_SIZE];

    g_return_if_fail (key_file != NULL);

    g_ascii_dtostr (result, sizeof (result), value);
    g_key_file_set_value (key_file, group_name, key, result);
}

gdouble*
g_key_file_get_double_list (GKeyFile *key_file,
                            const gchar *group_name,
                            const gchar *key,
                            gsize *length,
                            GError **error)
{
    GError *key_file_error = NULL;
    gchar **values;
    gdouble *double_values;
    gsize i, num_doubles;

    g_return_val_if_fail (key_file != NULL, NULL);
    g_return_val_if_fail (group_name != NULL, NULL);
    g_return_val_if_fail (key != NULL, NULL);

    values = g_key_file_get_string_list (key_file, group_name, key,
                                         &num_doubles, &key_file_error);

    if (key_file_error)
        g_propagate_error (error, key_file_error);

    if (!values)
        return NULL;

    double_values = g_new0 (gdouble, num_doubles);

    for (i = 0; i < num_doubles; i++)
    {
        double_values[i] = g_key_file_parse_value_as_double (key_file,
                           values[i],
                           &key_file_error);

        if (key_file_error)
        {
            g_propagate_error (error, key_file_error);
            g_strfreev (values);
            g_free (double_values);

            return NULL;
        }
    }
    g_strfreev (values);

    if (length)
        *length = num_doubles;

    return double_values;
}

void
g_key_file_set_double_list (GKeyFile     *key_file,
                            const gchar  *group_name,
                            const gchar  *key,
                            gdouble       list[],
                            gsize         length)
{
    GString *values;
    gsize i;

    g_return_if_fail (key_file != NULL);
    g_return_if_fail (list != NULL);

    values = g_string_sized_new (length * 16);
    for (i = 0; i < length; i++)
    {
        gchar result[G_ASCII_DTOSTR_BUF_SIZE];

        g_ascii_dtostr( result, sizeof (result), list[i] );

        g_string_append (values, result);
        g_string_append_c (values, ';');
    }

    g_key_file_set_value (key_file, group_name, key, values->str);
    g_string_free (values, TRUE);
}
/**********************************************************************
 *
 *                     End of copied functions.
 *
 **********************************************************************/
#endif


GKeyFile *
gnc_key_file_load_from_file (const gchar *filename,
                             gboolean ignore_error,
                             gboolean return_empty_struct,
                             GError **caller_error)
{
    GKeyFile *key_file;
    GError *error = NULL;

    g_return_val_if_fail(filename != NULL, NULL);

    if (!g_file_test(filename, G_FILE_TEST_EXISTS))
        return NULL;

    key_file = g_key_file_new();
    if (!key_file)
        return NULL;

    if (g_key_file_load_from_file(key_file, filename, G_KEY_FILE_NONE, &error))
        return key_file;

    /* An error occurred */
    if (!return_empty_struct)
    {
        g_key_file_free(key_file);
        key_file = NULL;
    }

    if (!ignore_error)
        g_warning("Unable to read file %s: %s\n", filename, error->message);
    g_propagate_error(caller_error, error);
    return key_file;
}


gboolean
gnc_key_file_save_to_file (const gchar *filename,
                           GKeyFile *key_file,
                           GError **error)
{
    gchar *contents;
    gint fd;
    extern int errno;
    gint length;
    ssize_t written;
    gboolean success = TRUE;

    g_return_val_if_fail(filename != NULL, FALSE);
    g_return_val_if_fail(key_file != NULL, FALSE);
    if (error)
        g_return_val_if_fail(*error == NULL, FALSE);

    contents = g_key_file_to_data(key_file, NULL, NULL);
    length = strlen(contents);
    fd = g_open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (fd == -1)
    {
        if (error)
        {
            *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno),
                                 "Cannot open file %s: %s", filename,
                                 strerror(errno));
        }
        else
        {
            g_critical("Cannot open file %s: %s\n", filename, strerror(errno));
        }
        g_free(contents);
        return FALSE;
    }

    written = write(fd, contents, length);
    if (written == -1 )
    {
        success = FALSE;
        if (error)
        {
            *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno),
                                 "Cannot write to file %s: %s", filename,
                                 strerror(errno));
        }
        else
        {
            g_critical("Cannot write to file %s: %s\n", filename, strerror(errno));
        }
        close(fd);
    }
    else if (written != length)
    {
        success = FALSE;
        if (error)
        {
            *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno),
                                 "File %s truncated (provided %d, written %d)",
                                 filename, length, (int)written);
        }
        else
        {
            g_critical("File %s truncated (provided %d, written %d)",
                       filename, length, (int)written);
        }
        /* Ignore any error */
        close(fd);
    }
    else if (close(fd) == -1)
    {
        if (error)
        {
            *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno),
                                 "Close failed for file %s: %s", filename,
                                 strerror(errno));
        }
        else
        {
            g_warning("Close failed for file %s: %s", filename, strerror(errno));
        }
    }
    g_free(contents);
    return success;
}

/** @} */
/** @} */
