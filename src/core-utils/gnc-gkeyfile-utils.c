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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
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
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include "gnc-gkeyfile-utils.h"

GKeyFile *
gnc_key_file_load_from_file (const gchar *filename, gboolean ignore_error)
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
  if (!ignore_error) {
    g_key_file_free(key_file);
    key_file = NULL;
  }

  g_warning("Unable to read file %s: %s\n", filename, error->message);
  g_error_free(error);
  return key_file;
}


gboolean
gnc_key_file_save_to_file (const gchar *filename,
			   GKeyFile *key_file)
{
  gchar *contents;
  gint fd;
  extern int errno;
  gint length;
  gboolean success = TRUE;

  contents = g_key_file_to_data(key_file, NULL, NULL);
  length = strlen(contents);
  if (length) {
    fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (fd != -1) {
      write(fd, contents, length);
      close(fd);
    } else {
      g_warning("Cannot open file %s: %s\n", filename, strerror(errno));
      success = FALSE;
    }
  } else {
    unlink(filename);
  }
  g_free(contents);
  return success;
}

/** @} */
/** @} */
