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
