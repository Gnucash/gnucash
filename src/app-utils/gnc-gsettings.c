/********************************************************************\
 * gnc-gsettings.c -- utility functions for storing/retrieving      *
 *              data in the GSettings database for GnuCash          *
 * Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>           *
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
#include "gnc-gsettings.h"

#define CLIENT_TAG  "%s-%s-client"
#define NOTIFY_TAG  "%s-%s-notify_id"

static GHashTable *schema_hash = NULL;
static const gchar *gsettings_prefix;

/************************************************************/
/*               Internal helper functions                  */
/************************************************************/
static gboolean gnc_gsettings_is_valid_key(GSettings *settings, const gchar *key)
{
    gchar **keys = NULL;
    gint i = 0;
    gboolean found = FALSE;

    // Check if the key is valid key within settings
    if (!G_IS_SETTINGS(settings))
        return FALSE;

    // Get list of keys
    keys = g_settings_list_keys(settings);

    while (keys && keys[i])
    {
        if (!g_strcmp0(key, keys[i]))
        {
            found = TRUE;
            break;
        }
        i++;
    }

    // Free keys
    g_strfreev(keys);

    return found;
}

static GSettings * gnc_gsettings_get_schema_ptr (const gchar *schema_str)
{
    GSettings *gset = NULL;
    gchar *full_name = gnc_gsettings_normalize_schema_name (schema_str);

    if (!schema_hash)
        schema_hash = g_hash_table_new (g_str_hash, g_str_equal);

    gset = g_hash_table_lookup (schema_hash, full_name);
    if (!gset)
    {
        gset = g_settings_new (full_name);
        if (G_IS_SETTINGS(gset))
            g_hash_table_insert (schema_hash, full_name, gset);
        else
            PWARN ("Ignoring attempt to access unknown gsettings schema %s", full_name);
    }

    g_free (full_name);
    return gset;
}



/************************************************************/
/*                      GSettings Utilities                 */
/************************************************************/

void
gnc_gsettings_set_prefix (const gchar *prefix)
{
    gsettings_prefix = prefix;
}

const gchar *
gnc_gsettings_get_prefix (void)
{
    return gsettings_prefix;
}

gchar *
gnc_gsettings_normalize_schema_name (const gchar *name)
{
    if (name == NULL)
    {
        /* Need to return a newly allocated string */
        return g_strdup(gnc_gsettings_get_prefix());
    }
    if (g_str_has_prefix (name, gnc_gsettings_get_prefix ()))
    {
        /* Need to return a newly allocated string */
        return g_strdup(name);
    }

    return g_strjoin(".", gnc_gsettings_get_prefix(), name, NULL);
}
