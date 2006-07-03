/********************************************************************\
 * gnc-glib-utils.c -- utility functions based on glib functions    *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
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
 *                                                                  *
\********************************************************************/

#include "config.h"
#include <stdio.h>
#include <string.h>

#include "gnc-glib-utils.h"

int 
safe_utf8_collate (const char * da, const char * db)
{
  if (da && !(*da))
    da = NULL;
  if (db && !(*db))
    db = NULL;

  if (da && db)
    return g_utf8_collate(da, db);
  if (da)
    return 1;
  if (db)
    return -1;
  return 0;
}

gboolean
gnc_utf8_validate (const gchar *str)
{
  return g_utf8_validate(str, -1, NULL);
}

void
gnc_utf8_strip_invalid (gchar *str)
{
  gchar *end;
  gint len;

  if (g_utf8_validate(str, -1, (const gchar **)&end))
    return;

  g_warning("Invalid utf8 string: %s", str);
  do {
    len = strlen(end);
    memmove(end, end+1, len);	/* shuffle the remainder one byte */
  } while (!g_utf8_validate(str, -1, (const gchar **)&end));
}
