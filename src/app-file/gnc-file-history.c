/********************************************************************\
 * gnc-file-history.c -- functions to maintain file history menu    *
 * Copyright (C) 2000 Robby Stephenson         	                    *
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

#define HISTORY_SECTION "/GnuCash/History/"

static GList *history_list = NULL;
static gnc_history_changed_cb history_changed_cb = NULL;

void
gnc_history_set_callback (gnc_history_changed_cb cb)
{
  history_changed_cb = cb;
}

static void
gnc_history_config_write (void)
{
  guint num_files, i;
  char key[20];
  GList *tmp;

  if (history_list == NULL)
    return;

  num_files = g_list_length(history_list);
  if (num_files == 0)
    return;

  gnome_config_clean_section (HISTORY_SECTION);
  gnome_config_push_prefix (HISTORY_SECTION);
  gnome_config_set_int ("MaxFiles", num_files);

  for (tmp = history_list, i = 0; tmp != NULL; tmp = g_list_next(tmp), i++)
  {
    g_sprintf(key, "File%d", i);
    gnome_config_set_string (key, tmp->data);
  }

  gnome_config_pop_prefix ();
  gnome_config_sync ();
}


void
gnc_history_init_list (void)
{
  int num_files, i;
  char key[20], *filename;

  gnome_config_push_prefix (HISTORY_SECTION);

  g_sprintf (key, "MaxFiles=%d", MAX_HISTORY_FILES);
  num_files = gnome_config_get_int (key);
  if (num_files > MAX_HISTORY_FILES)
    num_files = MAX_HISTORY_FILES;

  for (i = 0; i < num_files; i++)
  {
    g_sprintf (key, "File%d", i);
    filename = gnome_config_get_string (key);
    if (filename == NULL)
      continue;
    if (!g_utf8_validate(filename, -1, NULL))
      return;
    history_list = g_list_append (history_list, filename);
  }

  gnome_config_pop_prefix ();
}


void
gnc_history_add_file (const char *newfile)
{
  GList *tmp;

  if (newfile == NULL)
    return;
  if (!g_utf8_validate(newfile, -1, NULL))
    return;

  if (history_list == NULL)
    gnc_history_init_list ();

  /* See if its already there. If so, move to the top. */
  for (tmp = history_list; tmp; tmp = g_list_next(tmp)) {
    if (g_utf8_collate (newfile, tmp->data) == 0) {
      history_list = g_list_remove_link (history_list, tmp);
      history_list = g_list_concat (tmp, history_list);
      break;
    }
  }

  /* If not there, add a new item at the top. */
  if (tmp == NULL) {
      history_list = g_list_prepend (history_list, g_strdup(newfile));
  }

  /* Trim the list  if necessary*/
  if (g_list_length(history_list) > MAX_HISTORY_FILES) {
    tmp = g_list_last(history_list);
    g_free(tmp->data);
    history_list = g_list_delete_link(history_list, tmp);
  }

  /* Write the new list to disk */
  gnc_history_config_write ();

  /* Update the menus actions */
  if (history_changed_cb)
    history_changed_cb();
}


const GList *
gnc_history_get_file_list (void)
{
  if (history_list == NULL)
    gnc_history_init_list ();

  return history_list;
}

const char *
gnc_history_get_last (void)
{
  if (history_list == NULL)
    gnc_history_init_list ();

  if (history_list == NULL)
    return NULL;

  return g_list_last(history_list)->data;
}
