/********************************************************************\
 * gnc-file-history.c -- functions to maintain file history menu    *
 * Copyright (C) 2000 Robby Stephenson         	                    *
 * Copyright (C) 2005 David Hampton            	                    *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <glib/gprintf.h>
#include <gnome.h>
#include "gnc-file.h"
#include "gnc-file-history.h"
#include "gnc-gconf-utils.h"

gchar *
gnc_history_gconf_index_to_key (guint index)
{
  return g_strdup_printf(HISTORY_STRING_FILE_N, index);
}

gint
gnc_history_gconf_key_to_index (const gchar *fullkey)
{
  char *key;
  gint index, result;

  key = rindex(fullkey, '/');
  result = sscanf(key+1, HISTORY_STRING_FILE_N, &index);
  return (result == 1) ? index : -1;
}

void
gnc_history_add_file (const char *newfile)
{
  gchar *filename, *from, *to;
  gint i, last;

  if (newfile == NULL)
    return;
  if (!g_utf8_validate(newfile, -1, NULL))
    return;

  /*
   * Look for the filename in gconf.
   */
  last = MAX_HISTORY_FILES - 1;
  for (i = 0; i < MAX_HISTORY_FILES; i++) {
    from = g_strdup_printf(HISTORY_STRING_FILE_N, i);
    filename = gnc_gconf_get_string(HISTORY_STRING_SECTION, from, NULL);
    g_free(from);

    if (!filename) {
      last = i;
      break;
    }
    if (g_utf8_collate(newfile, filename) == 0) {
      g_free(filename);
      last = i;
      break;
    }
    g_free(filename);
  }

  /*
   * Shuffle filenames upward through gconf.
   */
  to = g_strdup_printf(HISTORY_STRING_FILE_N, last);
  for (i = last - 1; i >= 0; i--) {
    from = g_strdup_printf(HISTORY_STRING_FILE_N, i);
    filename = gnc_gconf_get_string(HISTORY_STRING_SECTION, from, NULL);
    if (filename) {
      gnc_gconf_set_string(HISTORY_STRING_SECTION, to, filename, NULL);
      g_free(filename);
    } else {
      gnc_gconf_unset(HISTORY_STRING_SECTION, to, NULL);
    }
    g_free(to);
    to = from;
  }

  /*
   * Store the new zero entry.
   */
  gnc_gconf_set_string(HISTORY_STRING_SECTION, to, newfile, NULL);
}


char *
gnc_history_get_last (void)
{
  static char *filename = NULL;
  char *key;

  /* The static string supports the current signature of this
   * function.  At some point this should be changed to pass the
   * allocated string up to the caller and make them responsible for
   * freeing irt, but that change percolates up into the scheme code
   * and requires changing that as well. */
  if (filename) {
    g_free(filename);
    filename = NULL;
  }

  key = g_strdup_printf(HISTORY_STRING_FILE_N, 0);
  filename = gnc_gconf_get_string(HISTORY_STRING_SECTION, key, NULL);
  g_free(key);

  return filename;
}
