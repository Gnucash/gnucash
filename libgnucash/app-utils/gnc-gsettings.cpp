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

#include <config.h>


#include <gio/gio.h>
#include <glib.h>

extern "C" {
#include <stdio.h>
#include <string.h>
#include "gnc-gsettings.h"
#include "gnc-path.h"
#include "qof.h"
#include "gnc-prefs-p.h"
}

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <fstream>
#include <iostream>

namespace bpt = boost::property_tree;

#define GSET_SCHEMA_PREFIX "org.gnucash.GnuCash"
#define GSET_SCHEMA_OLD_PREFIX "org.gnucash"
#define CLIENT_TAG  "%s-%s-client"
#define NOTIFY_TAG  "%s-%s-notify_id"

static GHashTable *schema_hash = NULL;
static const gchar *gsettings_prefix;

static GHashTable *registered_handlers_hash = NULL;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = "gnc.app-utils.gsettings";

/************************************************************/
/*               Internal helper functions                  */
/************************************************************/
static gboolean gnc_gsettings_is_valid_key(GSettings *settings, const gchar *key)
{
    gchar **keys = NULL;
    gint i = 0;
    gboolean found = FALSE;
    GSettingsSchema *schema;

    // Check if the key is valid key within settings
    if (!G_IS_SETTINGS(settings))
        return FALSE;

    g_object_get (settings, "settings-schema", &schema, NULL);
    if (!schema)
        return FALSE;

    keys = g_settings_schema_list_keys (schema);
    while (keys && keys[i])
    {
        if (!g_strcmp0(key, keys[i]))
        {
            found = TRUE;
            break;
        }
        i++;
    }
    g_strfreev(keys);

    return found;
}

static GSettings * gnc_gsettings_get_settings_ptr (const gchar *schema_str)
{
    GSettings *gset = NULL;
    gchar *full_name = gnc_gsettings_normalize_schema_name (schema_str);

    ENTER("");
    if (!schema_hash)
        schema_hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

    gset = static_cast<GSettings*> (g_hash_table_lookup (schema_hash, full_name));
    DEBUG ("Looking for schema %s returned gsettings %p", full_name, gset);

    if (!gset)
    {
        auto schema_source {g_settings_schema_source_get_default()};
        auto schema {g_settings_schema_source_lookup(schema_source, full_name,
                                                     TRUE)};
        gset = g_settings_new_full (schema, nullptr, nullptr);
        DEBUG ("Created gsettings object %p for schema %s", gset, full_name);
        if (G_IS_SETTINGS(gset))
            g_hash_table_insert (schema_hash, full_name, gset);
        else
            PWARN ("Ignoring attempt to access unknown gsettings schema %s", full_name);
    }
    else
    {
        g_free(full_name);
    }
    LEAVE("");
    return gset;
}

static void
handlers_hash_block_helper (gpointer key, gpointer settings_ptr, gpointer pointer)
{
    g_signal_handler_block (settings_ptr, (gulong)key); // block signal_handler
    PINFO("Block handler_id %ld for settings_ptr %p", (gulong)key, settings_ptr);
}

static void
handlers_hash_unblock_helper (gpointer key, gpointer settings_ptr, gpointer pointer)
{
    g_signal_handler_unblock (settings_ptr, (gulong)key); // unblock signal_handler
    PINFO("UnBlock handler_id %ld for settings_ptr %p", (gulong)key, settings_ptr);
}

/************************************************************/
/*                      GSettings Utilities                 */
/************************************************************/

const gchar *
gnc_gsettings_get_prefix (void)
{
    return GSET_SCHEMA_PREFIX;
}

gchar *
gnc_gsettings_normalize_schema_name (const gchar *name)
{
    if (!name)
    {
        /* Need to return a newly allocated string */
        return g_strdup(GSET_SCHEMA_PREFIX);
    }
    if (g_str_has_prefix (name, GSET_SCHEMA_PREFIX) ||
       (g_str_has_prefix (name, GSET_SCHEMA_OLD_PREFIX)))
    {
        /* Need to return a newly allocated string */
        return g_strdup(name);
    }

    return g_strjoin(".", GSET_SCHEMA_PREFIX, name, NULL);
}


/************************************************************/
/*                   Change notification                    */
/************************************************************/

gulong
gnc_gsettings_register_cb (const gchar *schema,
                           const gchar *key,
                           gpointer func,
                           gpointer user_data)
{
    gulong retval = 0;
    gchar *signal = NULL;

    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);

    ENTER("");
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), retval);
    g_return_val_if_fail (func, retval);

    if ((!key) || (*key == '\0'))
        signal = g_strdup ("changed");
    else
    {
        if (gnc_gsettings_is_valid_key(settings_ptr, key))
            signal = g_strconcat ("changed::", key, NULL);
    }

    retval = g_signal_connect (settings_ptr, signal, G_CALLBACK (func), user_data);

    if (!registered_handlers_hash)
        registered_handlers_hash = g_hash_table_new (g_direct_hash, g_direct_equal);

    if (retval != 0)
    {
        g_hash_table_insert (registered_handlers_hash,
                             GINT_TO_POINTER(retval), settings_ptr); //key, value

        PINFO("schema: %s, key: %s, settings_ptr: %p, handler_id: %ld",
               schema, key, settings_ptr, retval);
    }
    g_free (signal);

    LEAVE("");
    return retval;
}


void
gnc_gsettings_remove_cb_by_func (const gchar *schema,
                                 const gchar *key,
                                 gpointer func,
                                 gpointer user_data)
{
    gint matched = 0;
    GQuark quark = 0;
    gulong handler_id = 0;

    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_if_fail (G_IS_SETTINGS (settings_ptr));
    g_return_if_fail (func);

    ENTER ();

    if ((key) && (gnc_gsettings_is_valid_key(settings_ptr, key)))
        quark = g_quark_from_string (key);

    handler_id = g_signal_handler_find (
                  settings_ptr,
                  static_cast<GSignalMatchType> (G_SIGNAL_MATCH_DETAIL | G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA),
                  g_signal_lookup ("changed", G_TYPE_SETTINGS), /* signal_id */
                  quark,   /* signal_detail */
                  NULL, /* closure */
                  func, /* callback function */
                  user_data);

    while (handler_id)
    {
        matched ++;
        gnc_gsettings_remove_cb_by_id (schema, handler_id);

        handler_id = g_signal_handler_find (
                      settings_ptr,
                      static_cast<GSignalMatchType> (G_SIGNAL_MATCH_DETAIL | G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA),
                      g_signal_lookup ("changed", G_TYPE_SETTINGS), /* signal_id */
                      quark,   /* signal_detail */
                      NULL, /* closure */
                      func, /* callback function */
                      user_data);
    }

    LEAVE ("Schema: %s, key: %s, hashtable size: %d - removed %d handlers for 'changed' signal",
            schema, key, g_hash_table_size (registered_handlers_hash), matched);
}


void
gnc_gsettings_remove_cb_by_id (const gchar *schema,
                               guint handlerid)
{
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_if_fail (G_IS_SETTINGS (settings_ptr));

    ENTER ();

    g_signal_handler_disconnect (settings_ptr, handlerid);

    // remove the handlerid from the registerered_handlers_hash
    g_hash_table_remove (registered_handlers_hash, GINT_TO_POINTER(handlerid));

    // destroy hash table if size is 0
    if (g_hash_table_size (registered_handlers_hash) == 0)
    {
        g_hash_table_destroy (registered_handlers_hash);
        PINFO ("All registered preference callbacks removed");
    }

    LEAVE ("Schema: %s, handlerid: %d, hashtable size: %d - removed for handler",
            schema, handlerid, g_hash_table_size (registered_handlers_hash));
}


guint
gnc_gsettings_register_any_cb (const gchar *schema,
                               gpointer func,
                               gpointer user_data)
{
    return gnc_gsettings_register_cb (schema, NULL, func, user_data);
}


void
gnc_gsettings_remove_any_cb_by_func (const gchar *schema,
                                     gpointer func,
                                     gpointer user_data)
{
    gnc_gsettings_remove_cb_by_func (schema, NULL, func, user_data);
}


void gnc_gsettings_bind (const gchar *schema,
                         /*@ null @*/ const gchar *key,
                         gpointer object,
                         const gchar *property)
{
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_if_fail (G_IS_SETTINGS (settings_ptr));

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        g_settings_bind (settings_ptr, key, object, property, G_SETTINGS_BIND_DEFAULT);
    else
    {
        PERR ("Invalid key %s for schema %s", key, schema);
    }
}

/************************************************************/
/*                      Getters/Setters                     */
/************************************************************/

gboolean
gnc_gsettings_get_bool (const gchar *schema,
                        const gchar *key)
{
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        return g_settings_get_boolean (settings_ptr, key);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), FALSE);

    ENTER("schema: %s, key: %s", schema, key);
    if (gnc_gsettings_is_valid_key (settings_ptr, key))
    {
        result = g_settings_set_boolean (settings_ptr, key, value);
        if (!result)
            PERR ("Unable to set value for key %s in schema %s", key, schema);
    }
    else
        PERR ("Invalid key %s for schema %s", key, schema);

    LEAVE("result %i", result);
    return result;
}

gint
gnc_gsettings_get_int (const gchar *schema,
                       const gchar *key)
{
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), 0);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        return g_settings_get_int (settings_ptr, key);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
    {
        result = g_settings_set_int (settings_ptr, key, value);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), 0);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        return g_settings_get_double (settings_ptr, key);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
    {
        result = g_settings_set_double (settings_ptr, key, value);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), NULL);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        return g_settings_get_string (settings_ptr, key);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), FALSE);

    ENTER("schema: %s, key: %s", schema, key);
    if (gnc_gsettings_is_valid_key (settings_ptr, key))
    {
        result = g_settings_set_string (settings_ptr, key, value);
        if (!result)
            PERR ("Unable to set value for key %s in schema %s", key, schema);
    }
    else
        PERR ("Invalid key %s for schema %s", key, schema);

    LEAVE("result %i", result);
    return result;
}

gint
gnc_gsettings_get_enum (const gchar *schema,
                        const gchar *key)
{
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), 0);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        return g_settings_get_enum (settings_ptr, key);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
    {
        result = g_settings_set_enum (settings_ptr, key, value);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), NULL);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        return g_settings_get_value (settings_ptr, key);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), FALSE);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
    {
        result = g_settings_set_value (settings_ptr, key, value);
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
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_if_fail (G_IS_SETTINGS (settings_ptr));

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        g_settings_reset (settings_ptr, key);
    else
        PERR ("Invalid key %s for schema %s", key, schema);
}

void
gnc_gsettings_reset_schema (const gchar *schema_str)
{
    gchar **keys;
    gint counter = 0;
    GSettingsSchema *schema;
    GSettings *settings = gnc_gsettings_get_settings_ptr (schema_str);

    if (!settings)
        return;

    g_object_get (settings, "settings-schema", &schema, NULL);
    if (!schema)
        return;

    keys = g_settings_schema_list_keys (schema);
    if (!keys)
        return;

    while (keys[counter])
    {
        gnc_gsettings_reset (schema_str, keys[counter]);
        counter++;
    }

    g_strfreev (keys);
}

void gnc_gsettings_load_backend (void)
{
    ENTER("");

    /* The gsettings backend only works in an installed environment.
     * When called from the source environment (for testing purposes)
     * simply return.
     */
    if (g_strcmp0 (g_getenv ("GNC_UNINSTALLED"), "1") == 0)
        return;

    if (prefsbackend)
        g_free (prefsbackend);

    prefsbackend = g_new0 (PrefsBackend, 1);

    prefsbackend->register_cb = gnc_gsettings_register_cb;
    prefsbackend->remove_cb_by_func = gnc_gsettings_remove_cb_by_func;
    prefsbackend->remove_cb_by_id = gnc_gsettings_remove_cb_by_id;
    prefsbackend->register_group_cb = gnc_gsettings_register_any_cb;
    prefsbackend->remove_group_cb_by_func = gnc_gsettings_remove_any_cb_by_func;
    prefsbackend->bind = gnc_gsettings_bind;
    prefsbackend->get_bool = gnc_gsettings_get_bool;
    prefsbackend->get_int = gnc_gsettings_get_int;
    prefsbackend->get_float = gnc_gsettings_get_float;
    prefsbackend->get_string = gnc_gsettings_get_string;
    prefsbackend->get_enum = gnc_gsettings_get_enum;
    prefsbackend->get_value = gnc_gsettings_get_value;
    prefsbackend->set_bool = gnc_gsettings_set_bool;
    prefsbackend->set_int = gnc_gsettings_set_int;
    prefsbackend->set_float = gnc_gsettings_set_float;
    prefsbackend->set_string = gnc_gsettings_set_string;
    prefsbackend->set_enum = gnc_gsettings_set_enum;
    prefsbackend->set_value = gnc_gsettings_set_value;
    prefsbackend->reset = gnc_gsettings_reset;
    prefsbackend->reset_group = gnc_gsettings_reset_schema;
    prefsbackend->block_all = gnc_gsettings_block_all;
    prefsbackend->unblock_all = gnc_gsettings_unblock_all;

    /* Run any data model changes for the backend before it's used
     * by anyone */
    gnc_gsettings_version_upgrade();

    LEAVE("Prefsbackend bind = %p", prefsbackend->bind);
}



static GVariant *
gnc_gsettings_get_user_value (const gchar *schema,
                         const gchar *key)
{
    GSettings *settings_ptr = gnc_gsettings_get_settings_ptr (schema);
    g_return_val_if_fail (G_IS_SETTINGS (settings_ptr), NULL);

    if (gnc_gsettings_is_valid_key (settings_ptr, key))
        return g_settings_get_user_value (settings_ptr, key);
    else
    {
        PERR ("Invalid key %s for schema %s", key, schema);
        return NULL;
    }
}

using opt_str_vec = boost::optional<std::string>;

static void
deprecate_one_key (const opt_str_vec &oldpath, const opt_str_vec &oldkey)
{
    if (!oldpath || !oldkey )
    {
        DEBUG ("Skipping <deprecate> node - missing attribute (old-path or old-key)");
        return;
    }

    PINFO ("'%s:%s' has been marked deprecated", oldpath->c_str(), oldkey->c_str());
    /* This does nothing really, but is a reminder for future maintainers
     * to mark this pref as obsolete in the next major release. */
}

static void
migrate_one_key (const opt_str_vec &oldpath, const opt_str_vec &oldkey,
                 const opt_str_vec &newpath, const opt_str_vec &newkey)
{
    if (!oldpath || !oldkey || !newpath || !newkey)
    {
        DEBUG ("Skipping <migrate> node - missing attribute (old-path, old-key, new-path or new-key)");
        return;
    }

    PINFO ("Migrating '%s:%s' to '%s:%s'", oldpath->c_str(), oldkey->c_str(),
           newpath->c_str(), newkey->c_str());

    auto user_value = gnc_gsettings_get_user_value (oldpath->c_str(), oldkey->c_str());
    if (user_value)
        gnc_gsettings_set_value (newpath->c_str(), newkey->c_str(), user_value);
}

static void
obsolete_one_key (const opt_str_vec &oldpath, const opt_str_vec &oldkey)
{
    if (!oldpath || !oldkey )
    {
        DEBUG ("Skipping <obsolete> node - missing attribute (old-path or old-key)");
        return;
    }

    PINFO ("Resetting obsolete '%s:%s'", oldpath->c_str(), oldkey->c_str());
    gnc_gsettings_reset (oldpath->c_str(), oldkey->c_str());
}

static void
parse_one_release_node (bpt::ptree &pt)
{
    /* loop over top-level property tree */
    std::for_each (pt.begin(), pt.end(),
            [] (std::pair<bpt::ptree::key_type, bpt::ptree> node)
            {
                if (node.first == "<xmlattr>")
                    return;
                else if (node.first == "deprecate")
                    deprecate_one_key (node.second.get_optional<std::string> ("<xmlattr>.old-path"),
                                       node.second.get_optional<std::string> ("<xmlattr>.old-key"));
                else if (node.first == "migrate")
                    migrate_one_key (node.second.get_optional<std::string> ("<xmlattr>.old-path"),
                                     node.second.get_optional<std::string> ("<xmlattr>.old-key"),
                                     node.second.get_optional<std::string> ("<xmlattr>.new-path"),
                                     node.second.get_optional<std::string> ("<xmlattr>.new-key"));
                else if (node.first == "obsolete")
                    obsolete_one_key (node.second.get_optional<std::string> ("<xmlattr>.old-path"),
                                      node.second.get_optional<std::string> ("<xmlattr>.old-key"));
                else
                {
                    DEBUG ("Skipping unknown node <%s>", node.first.c_str());
                    return;
                }
            });
}

static void
transform_settings (int old_maj_min)
{
    bpt::ptree pt;

    auto pkg_data_dir = gnc_path_get_pkgdatadir();
    auto transform_file = std::string (pkg_data_dir) + "/pref_transformations.xml";
    g_free (pkg_data_dir);

    std::ifstream transform_stream {transform_file};
    if (!transform_stream.is_open())
    {
        PWARN("Failed to load preferences transformation file '%s'", transform_file.c_str());
        return;
    }

    try
    {
        bpt::read_xml (transform_stream, pt);
    }
    catch (bpt::xml_parser_error &e) {
        PWARN ("Failed to parse GnuCash preferences transformation file.\n");
        PWARN ("Error message:\n");
        PWARN ("%s\n", e.what());
        return;
    }
    catch (...) {
        PWARN ("Unknown error while parsing GnuCash preferences transformation file.\n");
        return;
    }

    /* loop over top-level property tree */
    std::for_each (pt.begin(), pt.end(),
            [&old_maj_min] (std::pair<bpt::ptree::key_type, bpt::ptree> node)
            {
                if (node.first != "release")
                {
                    DEBUG ("Skipping non-<release> node <%s>", node.first.c_str());
                    return;
                }
                auto version = node.second.get_optional<int> ("<xmlattr>.version");
                if (!version)
                {
                    DEBUG ("Skipping <release> node - no version attribute found");
                    return;
                }
                if (*version <= old_maj_min)
                {
                    DEBUG ("Skipping <release> node - version %i is less than current compatibility level %i", *version, old_maj_min);
                    return;
                }
                DEBUG ("Retrieved version value '%i'", *version);

                parse_one_release_node (node.second);
            });
}

void gnc_gsettings_version_upgrade (void)
{
    /* This routine will conditionally execute conversion rules from
     * prefs_transformations.xml to adapt the user's existing preferences to
     * the current preferences schema. The rules in this file are versioned and
     * only rules still relevant to the user's existing preferences and for
     * this version of GnuCash will be executed.
     *
     * Starting with GnuCash 4.7 the code expects all preferences to be stored
     * under prefix org.gnucash instead of org.gnucash.GnuCash, including our
     * GNC_PREF_VERSION setting.
     * As the logic to determine whether or not settings conversions are needed
     * depends on this preference, we have to test for its value in two
     * locations:
     * - if GNC_PREF_VERSION is not set under old nor new prefix
     *   => GnuCash has never run before so no conversion run necessary
     * - if GNC_PREF_VERSION is set under old prefix and not new prefix
     *   => user's preferences weren't moved yet from old to new prefix. Use old
     *      prefix GNC_PREF_VERSION to determine which conversions may be needed
     * - if GNC_PREF_VERSION is set under both prefixes
     *   => ignore old prefix and use new prefix GNC_PREF_VERSION to determine
     *      which conversions may be needed.
     * Sometime in the future (GnuCash 6.0) the old prefix will be fully removed
     * and the test will be simplified to only check in the new prefix.
     */
    ENTER("Start of settings transform routine.");

    auto ogG_maj_min = gnc_gsettings_get_user_value (GNC_PREFS_GROUP_GENERAL, GNC_PREF_VERSION);
    auto og_maj_min = gnc_gsettings_get_user_value (GSET_SCHEMA_OLD_PREFIX "." GNC_PREFS_GROUP_GENERAL, GNC_PREF_VERSION);

    if (!ogG_maj_min && !og_maj_min)
    {
        LEAVE("");
        return;
    }

    auto old_maj_min = 0;
    if (!ogG_maj_min)
        old_maj_min = gnc_gsettings_get_int (GSET_SCHEMA_OLD_PREFIX "." GNC_PREFS_GROUP_GENERAL, GNC_PREF_VERSION);
    else
    {
        g_variant_unref (ogG_maj_min);
        old_maj_min = gnc_gsettings_get_int (GNC_PREFS_GROUP_GENERAL, GNC_PREF_VERSION);
    }
    if (og_maj_min)
        g_variant_unref (og_maj_min);

    PINFO ("Previous setting compatibility level: %i", old_maj_min);

    transform_settings (old_maj_min);

    /* Only write current version if it's more recent than what was set */
    auto cur_maj_min = PROJECT_VERSION_MAJOR * 1000 + PROJECT_VERSION_MINOR;
    if (cur_maj_min > old_maj_min)
        gnc_gsettings_set_int (GNC_PREFS_GROUP_GENERAL, GNC_PREF_VERSION, cur_maj_min);

    LEAVE("");
}


void gnc_gsettings_block_all (void)
{
    PINFO("block registered_handlers_hash list size is %d",
           g_hash_table_size (registered_handlers_hash));
    g_hash_table_foreach (registered_handlers_hash,
                          handlers_hash_block_helper, NULL);
}


void gnc_gsettings_unblock_all (void)
{
    PINFO("unblock registered_handlers_hash list size is %d",
           g_hash_table_size (registered_handlers_hash));
    g_hash_table_foreach (registered_handlers_hash,
                          handlers_hash_unblock_helper, NULL);
}
