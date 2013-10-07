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
#include "libqof/qof/qof.h"

#define CLIENT_TAG  "%s-%s-client"
#define NOTIFY_TAG  "%s-%s-notify_id"

static GHashTable *schema_hash = NULL;
static const gchar *gsettings_prefix;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

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


/************************************************************/
/*                   Change notification                    */
/************************************************************/

gulong
gnc_gsettings_register_cb (const gchar *schema,
                           const gchar *key,
                           GCallback func,
                           gpointer user_data)
{
    gulong retval = 0;
    gchar *signal = NULL;

    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), retval);

    if ((!key) || (*key == '\0'))
        signal = g_strdup ("changed");
    else
    {
        if (gnc_gsettings_is_valid_key(schema_ptr, key))
            signal = g_strconcat ("changed::", key, NULL);
    }

    retval = g_signal_connect (schema_ptr, signal, func, user_data);

    g_free (signal);

    return retval;
}


void
gnc_gsettings_remove_cb_by_func (const gchar *schema,
                                 const gchar *key,
                                 GCallback func,
                                 gpointer user_data)
{
    gint matched = 0;
    gchar *signal = NULL;

    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_if_fail (G_IS_SETTINGS (schema_ptr));

    if ((!key) || (*key == '\0'))
        signal = g_strdup ("changed");
    else
    {
        if (gnc_gsettings_is_valid_key(schema_ptr, key))
            signal = g_strconcat ("changed::", key, NULL);
    }

    matched = g_signal_handlers_disconnect_matched (
            schema_ptr,
            G_SIGNAL_MATCH_DETAIL ||G_SIGNAL_MATCH_FUNC || G_SIGNAL_MATCH_DATA,
            0, /* signal_id */
            g_quark_from_string (signal),   /* signal_detail */
            NULL, /* closure */
            func, /* callback function */
            user_data);
    DEBUG ("Removed %d handlers for signal '%s' from schema '%s'", matched, signal, schema);

    g_free (signal);
}


void
gnc_gsettings_remove_cb_by_id (const gchar *schema,
                               guint handlerid)
{
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_if_fail (G_IS_SETTINGS (schema_ptr));

    g_signal_handler_disconnect (schema_ptr, handlerid);
}


guint
gnc_gsettings_register_any_cb (const gchar *schema,
                               GCallback func,
                               gpointer user_data)
{
    return gnc_gsettings_register_cb (schema, NULL, func, user_data);
}


void
gnc_gsettings_remove_any_cb_by_func (const gchar *schema,
                                     GCallback func,
                                     gpointer user_data)
{
    gnc_gsettings_remove_cb_by_func (schema, NULL, func, user_data);
}


/************************************************************/
/*                      Getters/Setters                     */
/************************************************************/

gboolean
gnc_gsettings_get_bool (const gchar *schema,
                        const gchar *key)
{
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
        return g_settings_get_boolean (schema_ptr, key);
    else
    {
        PERR ("Invalid key %s for schema %s", key, schema);
        return FALSE;
    }
}

gboolean
gnc_gsettings_set_bool (const gchar *schema,
                        const gchar *key,
                        gboolean value)
{
    gboolean result = FALSE;
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
    {
        result = g_settings_set_boolean (schema_ptr, key, value);
        if (!result)
            PERR ("Unable to set value for key %s in schema %s", key, schema);
    }
    else
        PERR ("Invalid key %s for schema %s", key, schema);

    return result;
}

gint
gnc_gsettings_get_int (const gchar *schema,
                       const gchar *key)
{
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), 0);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
        return g_settings_get_int (schema_ptr, key);
    else
    {
        PERR ("Invalid key %s for schema %s", key, schema);
        return 0;
    }
}

gboolean
gnc_gsettings_set_int (const gchar *schema,
                       const gchar *key,
                       gint value)
{
    gboolean result = FALSE;
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
    {
        result = g_settings_set_int (schema_ptr, key, value);
        if (!result)
            PERR ("Unable to set value for key %s in schema %s", key, schema);
    }
    else
        PERR ("Invalid key %s for schema %s", key, schema);

    return result;
}

gdouble
gnc_gsettings_get_float (const gchar *schema,
                         const gchar *key)
{
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), 0);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
        return g_settings_get_double (schema_ptr, key);
    else
    {
        PERR ("Invalid key %s for schema %s", key, schema);
        return 0;
    }
}

gboolean
gnc_gsettings_set_float (const gchar *schema,
                         const gchar *key,
                         gdouble value)
{
    gboolean result = FALSE;
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
    {
        result = g_settings_set_double (schema_ptr, key, value);
        if (!result)
            PERR ("Unable to set value for key %s in schema %s", key, schema);
    }
    else
        PERR ("Invalid key %s for schema %s", key, schema);

    return result;
}

gchar *
gnc_gsettings_get_string (const gchar *schema,
                          const gchar *key)
{
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), NULL);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
        return g_settings_get_string (schema_ptr, key);
    else
    {
        PERR ("Invalid key %s for schema %s", key, schema);
        return NULL;
    }
}

gboolean
gnc_gsettings_set_string (const gchar *schema,
                          const gchar *key,
                          const gchar *value)
{
    gboolean result = FALSE;
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
    {
        result = g_settings_set_string (schema_ptr, key, value);
        if (!result)
            PERR ("Unable to set value for key %s in schema %s", key, schema);
    }
    else
        PERR ("Invalid key %s for schema %s", key, schema);

    return result;
}

gint
gnc_gsettings_get_enum (const gchar *schema,
                        const gchar *key)
{
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), 0);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
        return g_settings_get_enum (schema_ptr, key);
    else
    {
        PERR ("Invalid key %s for schema %s", key, schema);
        return 0;
    }
}

gboolean
gnc_gsettings_set_enum (const gchar *schema,
                        const gchar *key,
                        gint value)
{
    gboolean result = FALSE;
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
    {
        result = g_settings_set_enum (schema_ptr, key, value);
        if (!result)
            PERR ("Unable to set value for key %s in schema %s", key, schema);
    }
    else
        PERR ("Invalid key %s for schema %s", key, schema);

    return result;
}

GVariant *
gnc_gsettings_get_value (const gchar *schema,
                         const gchar *key)
{
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), NULL);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
        return g_settings_get_value (schema_ptr, key);
    else
    {
        PERR ("Invalid key %s for schema %s", key, schema);
        return NULL;
    }
}

gboolean
gnc_gsettings_set_value (const gchar *schema,
                         const gchar *key,
                         GVariant *value)
{
    gboolean result = FALSE;
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (schema_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
    {
        result = g_settings_set_value (schema_ptr, key, value);
        if (!result)
            PERR ("Unable to set value for key %s in schema %s", key, schema);
    }
    else
        PERR ("Invalid key %s for schema %s", key, schema);

    return result;
}

void
gnc_gsettings_reset (const gchar *schema,
                     const gchar *key)
{
    GSettings *schema_ptr = gnc_gsettings_get_schema_ptr (schema);
    g_return_if_fail (G_IS_SETTINGS (schema_ptr));

    if (gnc_gsettings_is_valid_key (schema_ptr, key))
        g_settings_reset (schema_ptr, key);
    else
        PERR ("Invalid key %s for schema %s", key, schema);
}

void
gnc_gsettings_reset_schema (const gchar *schema)
{
    gchar **keys;
    gint counter = 0;

    keys = g_settings_list_keys (gnc_gsettings_get_schema_ptr (schema));

    if (!keys)
        return;

    while (keys[counter])
    {
        gnc_gsettings_reset (schema, keys[counter]);
        counter++;
    }

    g_strfreev (keys);
}
