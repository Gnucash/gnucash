/********************************************************************\
 * gnc-gconf-utils.c -- utility functions for gnome for GnuCash     *
 * Copyright (C) 2001 Linux Developers Group                        *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <gconf/gconf-client.h>

#include "gnc-gconf-utils.h"


char *
gnc_gconf_section_name (const char *name)
{
  return gnome_gconf_get_app_settings_relative(NULL, name);
}

static gchar *
gnc_gconf_make_key (gchar *key, const gchar *section, const gchar *name)
{
  if (key)
    return key;
  return g_strdup_printf("%s/%s", section, name);
}


gboolean
gnc_gconf_client_get_bool (GConfClient *client,
			   gchar *key,
			   const gchar *section,
			   const gchar *name,
			   gboolean *value)
{
  GError *error = NULL;

  if (client == NULL)
    client = gconf_client_get_default ();

  key = gnc_gconf_make_key (key, section, name);
  *value = gconf_client_get_bool (client, key, &error);
  if (error == NULL) {
    g_free(key);
    return TRUE;
  }

  printf("Failed to load key %s: %s", key, error->message);
  g_error_free (error);
  g_free(key);
  return FALSE;
}

void
gnc_gconf_client_set_bool (GConfClient *client,
			   gchar *key,
			   const gchar *section,
			   const gchar *name,
			   const gboolean value)
{
  GError *error = NULL;

  if (client == NULL)
    client = gconf_client_get_default ();

  /* Remember whether the column width */
  key = gnc_gconf_make_key (key, section, name);
  if (!gconf_client_set_bool (client, key, value, &error)) {
    printf("Failed to save key %s: %s", key, error->message);
    g_error_free (error);
  }
  g_free(key);
}

gboolean
gnc_gconf_client_get_int (GConfClient *client,
			  gchar *key,
			  const gchar *section,
			  const gchar *name,
			  gint *value)
{
  GError *error = NULL;

  if (client == NULL)
    client = gconf_client_get_default ();

  key = gnc_gconf_make_key (key, section, name);
  *value = gconf_client_get_int (client, key, &error);
  if (error == NULL) {
    g_free(key);
    return TRUE;
  }

  printf("Failed to load key %s: %s", key, error->message);
  g_error_free (error);
  g_free(key);
  return FALSE;
}

void
gnc_gconf_client_set_int (GConfClient *client,
			  gchar *key,
			  const gchar *section,
			  const gchar *name,
			  const gint value)
{
  GError *error = NULL;

  if (client == NULL)
    client = gconf_client_get_default ();

  /* Remember whether the column width */
  key = gnc_gconf_make_key (key, section, name);
  if (!gconf_client_set_int (client, key, value, &error)) {
    printf("Failed to save key %s: %s", key, error->message);
    g_error_free (error);
  }
  g_free(key);
}

gboolean
gnc_gconf_client_get_string (GConfClient *client,
			   gchar *key,
			   const gchar *section,
			   const gchar *name,
			   gchar **value)
{
  GError *error = NULL;

  if (client == NULL)
    client = gconf_client_get_default ();

  key = gnc_gconf_make_key (key, section, name);
  *value = gconf_client_get_string (client, key, &error);
  if (error == NULL) {
    g_free(key);
    return TRUE;
  }

  printf("Failed to load key %s: %s", key, error->message);
  g_error_free (error);
  g_free(key);
  return FALSE;

}

void
gnc_gconf_client_set_string (GConfClient *client,
			     gchar *key,
			     const gchar *section,
			     const gchar *name,
			     const gchar *value)
{
  GError *error = NULL;

  key = gnc_gconf_make_key (key, section, name);
  if (!gconf_client_set_string (client, key, value, &error)) {
    printf("Failed to save key %s: %s", key, error->message);
    g_error_free (error);
  }
  g_free(key);
}
