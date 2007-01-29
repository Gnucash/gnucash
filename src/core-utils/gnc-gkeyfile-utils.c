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
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include "gnc-gkeyfile-utils.h"

GKeyFile *
gnc_key_file_load_from_file (const gchar *filename,
			     gboolean ignore_error,
			     gboolean return_empty_struct)
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
  if (!return_empty_struct) {
    g_key_file_free(key_file);
    key_file = NULL;
  }

  if (!ignore_error)
    g_warning("Unable to read file %s: %s\n", filename, error->message);
  g_error_free(error);
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
  fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd == -1) {
    if (error) {
      *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), 
			   "Cannot open file %s: %s", filename,
			   strerror(errno));
    } else {
      g_critical("Cannot open file %s: %s\n", filename, strerror(errno));
    }
    g_free(contents);
    return FALSE;
  }

  written = write(fd, contents, length);
  if (written == -1 ) {
    success = FALSE;
    if (error) {
      *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), 
			   "Cannot write to file %s: %s", filename,
			   strerror(errno));
    } else {
      g_critical("Cannot write to file %s: %s\n", filename, strerror(errno));
    }
    close(fd);
  } else if (written != length) {
    success = FALSE;
    if (error) {
      *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), 
			   "File %s truncated (provided %d, written %d)",
			   filename, length, (int)written);
    } else {
      g_critical("File %s truncated (provided %d, written %d)",
	      filename, length, (int)written);
    }
    /* Ignore any error */
    close(fd);
  } else if (close(fd) == -1) {
    if (error) {
      *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), 
			   "Close failed for file %s: %s", filename,
			   strerror(errno));
    } else {
      g_warning("Close failed for file %s: %s", filename, strerror(errno));
    }
  }
  g_free(contents);
  return success;
}


/* Compatability functions to handle reading key names both with and
 * without embedded spaces.  The 2.0 release uses names containing
 * spaces, while 2.2 uses names without spaces. These functions allow
 * you to fall back to using 2.0 after trying a 2.2 release.  */ 

static gchar *
gnc_key_file_translate_key (const gchar *key)
{
  gchar **parts, *part, *newkey;
  gint j;

  parts = g_strsplit(key, " ", -1);
  for (j = 0, part = parts[j++]; part; part = parts[j++])
    part[0] = g_ascii_toupper(part[0]);
  newkey = g_strjoinv("", parts);
  g_strfreev(parts);

  return newkey;
}


gboolean
gnc_key_file_get_boolean (GKeyFile *key_file,
			  const gchar *group_name,
			  const gchar *key,
			  GError **error)
{
  gchar *new_key;
  gboolean result;
  GError *local_error = NULL;

  result = g_key_file_get_boolean (key_file, group_name, key, &local_error);
  if (local_error == NULL)
    return result;
  if ((local_error->domain == G_KEY_FILE_ERROR) &&
      (local_error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)) {
    g_clear_error(&local_error);
    new_key = gnc_key_file_translate_key(key);
    result = g_key_file_get_boolean (key_file, group_name, new_key, &local_error);
    g_free(new_key);
  }
  if (local_error)
    g_propagate_error(error, local_error);
  return result;
}

gint
gnc_key_file_get_integer (GKeyFile *key_file,
			  const gchar *group_name,
			  const gchar *key,
			  GError **error)
{
  gchar *new_key;
  gint result;
  GError *local_error = NULL;

  result = g_key_file_get_integer (key_file, group_name, key, &local_error);
  if (local_error == NULL)
    return result;
  if ((local_error->domain == G_KEY_FILE_ERROR) &&
      (local_error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)) {
    g_clear_error(&local_error);
    new_key = gnc_key_file_translate_key(key);
    result = g_key_file_get_integer (key_file, group_name, new_key, &local_error);
    g_free(new_key);
  }
  if (local_error)
    g_propagate_error(error, local_error);
  return result;
}

gint *
gnc_key_file_get_integer_list (GKeyFile *key_file,
			       const gchar *group_name,
			       const gchar *key,
			       gsize *length,
			       GError **error)
{
  gchar *new_key;
  gint *result;
  GError *local_error = NULL;

  result = g_key_file_get_integer_list (key_file, group_name, key, length, &local_error);
  if (local_error == NULL)
    return result;
  if ((local_error->domain == G_KEY_FILE_ERROR) &&
      (local_error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)) {
    g_clear_error(&local_error);
    new_key = gnc_key_file_translate_key(key);
    result = g_key_file_get_integer_list (key_file, group_name, new_key, length, &local_error);
    g_free(new_key);
  }
  if (local_error)
    g_propagate_error(error, local_error);
  return result;
}

gchar *
gnc_key_file_get_string (GKeyFile *key_file,
			 const gchar *group_name,
			 const gchar *key,
			 GError **error)
{
  gchar *new_key;
  gchar *result;
  GError *local_error = NULL;

  result = g_key_file_get_string (key_file, group_name, key, &local_error);
  if (local_error == NULL)
    return result;
  if ((local_error->domain == G_KEY_FILE_ERROR) &&
      (local_error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)) {
    g_clear_error(&local_error);
    new_key = gnc_key_file_translate_key(key);
    result = g_key_file_get_string (key_file, group_name, new_key, &local_error);
    g_free(new_key);
  }
  if (local_error)
    g_propagate_error(error, local_error);
  return result;
}


/** @} */
/** @} */
