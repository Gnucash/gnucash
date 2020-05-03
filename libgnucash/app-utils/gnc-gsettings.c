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

#include <stdio.h>
#include <string.h>
#include "gnc-gsettings.h"
#include "gnc-path.h"
#include "guile-mappings.h"
#include <libguile.h>
#include "qof.h"
#include "gnc-prefs-p.h"

#include <libxml/xmlmemory.h>
#include <libxml/debugXML.h>
#include <libxml/HTMLtree.h>
#include <libxml/xmlIO.h>
#include <libxml/xinclude.h>
#include <libxml/catalog.h>
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>

#define CLIENT_TAG  "%s-%s-client"
#define NOTIFY_TAG  "%s-%s-notify_id"

#define GNC_PREF_MIGRATE_PREFS_DONE "migrate-prefs-done"

static GHashTable *schema_hash = NULL;
static const gchar *gsettings_prefix;
static xmlExternalEntityLoader defaultEntityLoader = NULL;

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
#ifdef HAVE_GLIB_2_46
    GSettingsSchema *schema;
#endif

    // Check if the key is valid key within settings
    if (!G_IS_SETTINGS(settings))
        return FALSE;

#ifdef HAVE_GLIB_2_46
    g_object_get (settings, "settings-schema", &schema, NULL);

    if (!schema)
        return FALSE;
#endif

    // Get list of keys
#ifdef HAVE_GLIB_2_46
    keys = g_settings_schema_list_keys(schema);
#else
    keys = g_settings_list_keys(settings);
#endif

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

static GSettings * gnc_gsettings_get_settings_ptr (const gchar *schema_str)
{
    GSettings *gset = NULL;
    gchar *full_name = gnc_gsettings_normalize_schema_name (schema_str);

    ENTER("");
    if (!schema_hash)
        schema_hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

    gset = g_hash_table_lookup (schema_hash, full_name);
    DEBUG ("Looking for schema %s returned gsettings %p", full_name, gset);
    if (!gset)
    {
        gset = g_settings_new (full_name);
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

void
gnc_gsettings_set_prefix (const gchar *prefix)
{
    gsettings_prefix = prefix;
}

const gchar *
gnc_gsettings_get_prefix (void)
{
    if (!gsettings_prefix)
    {
        const char *prefix = g_getenv("GNC_GSETTINGS_PREFIX");
        if (prefix)
            gsettings_prefix = prefix;
        else
            gsettings_prefix = GSET_SCHEMA_PREFIX;
    }
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
                  G_SIGNAL_MATCH_DETAIL | G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA,
                  g_signal_lookup ("changed", G_TYPE_SETTINGS), /* signal_id */
                  quark,   /* signal_detail */
                  NULL, /* closure */
                  G_CALLBACK (func), /* callback function */
                  user_data);

    while (handler_id)
    {
        matched ++;
        gnc_gsettings_remove_cb_by_id (schema, handler_id);

        handler_id = g_signal_handler_find (
                      settings_ptr,
                      G_SIGNAL_MATCH_DETAIL | G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA,
                      g_signal_lookup ("changed", G_TYPE_SETTINGS), /* signal_id */
                      quark,   /* signal_detail */
                      NULL, /* closure */
                      G_CALLBACK (func), /* callback function */
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
        g_settings_bind (settings_ptr, key, object, property, 0);
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

#ifdef HAVE_GLIB_2_46
    GSettingsSchema *schema;
#endif
    GSettings *settings = gnc_gsettings_get_settings_ptr (schema_str);

    if (!settings)
        return;

#ifdef HAVE_GLIB_2_46
    g_object_get (settings, "settings-schema", &schema, NULL);

    if (!schema)
        return;

    keys = g_settings_schema_list_keys (schema);
#else
    keys = g_settings_list_keys (settings);
#endif


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

    if (!prefsbackend)
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

    LEAVE("Prefsbackend bind = %p", prefsbackend->bind);
}

/* Attempt to migrate preferences from gconf files
    to gsettings if not already done so */

/* This snippet is borrowed from the xsltproc source
 * and adapted to help the xsl transform find our temporary
 * files in $HOME/.gnc-migration-tmp/
 */
static xmlParserInputPtr
xsltprocExternalEntityLoader(const char *URL, const char *ID,
                             xmlParserCtxtPtr ctxt)
{
    xmlParserInputPtr ret;
    warningSAXFunc warning = NULL;
    xmlChar *newURL;
    gchar *tmpdir = g_build_filename (g_get_home_dir (), ".gnc-migration-tmp", NULL);

    const char *lastsegment = URL;
    const char *iter = URL;

    while (*iter != 0)
    {
        if (*iter == '/')
            lastsegment = iter + 1;
        iter++;
    }

    if ((ctxt != NULL) && (ctxt->sax != NULL))
    {
        warning = ctxt->sax->warning;
        ctxt->sax->warning = NULL;
    }

    if (defaultEntityLoader != NULL)
    {
        ret = defaultEntityLoader(URL, ID, ctxt);
        if (ret != NULL)
        {
            if (warning != NULL)
                ctxt->sax->warning = warning;
            return(ret);
        }
    }

    newURL = xmlStrdup((const xmlChar *) tmpdir);
    newURL = xmlStrcat(newURL, (const xmlChar *) "/");
    newURL = xmlStrcat(newURL, (const xmlChar *) lastsegment);
    g_free (tmpdir);
    if (newURL != NULL)
    {
        ret = defaultEntityLoader((const char *)newURL, ID, ctxt);
        if (ret != NULL)
        {
            if (warning != NULL)
                ctxt->sax->warning = warning;
            xmlFree(newURL);
            return(ret);
        }
        xmlFree(newURL);
    }
    if (warning != NULL)
    {
        ctxt->sax->warning = warning;
        if (URL != NULL)
            DEBUG ("External entity \"%s\" not loaded", URL);
        else if (ID != NULL)
            DEBUG ("External entity \"%s\" not loaded", ID);
    }
    return(NULL);
}

/* Tool to migrate existing user settings from GConf to GSettings
 *
 * This tool will first run some sanity checks to see if migration
 * is necessary/possible. The actual migration works directly from
 * the GConf .xml files. Using an xsl transform it will convert them
 * in a guile script to set most settings found.
 *
 * Notes:
 * - due to some limitations in the xslt code, all the gconf xml files are
 *   first copied into a temporary directory. After the migration has finished,
 *   that temporary directory and its contents are removed.
 * - not all settings can be migrated. All the important ones are though.
 *   The ones that are missing are mostly with respect to window position
 *   and size.
 * - column widths/visibilities, sorting orders,... are no longer stored
 *   in gsettings, so these will obviously not be migrated either.
 * - upon a successful run, a flag will be set to prevent the migration
 *   from running again. So in normal circumstances the migration will
 *   be executed only once.
 */
static void gnc_gsettings_migrate_from_gconf (void)
{
    gchar *pkgdatadir, *stylesheet, *input, *output, *command;
    gchar *gconf_root, *gconf_apps, *gconf_gnucash;
    gchar *base_dir, *iter;
    SCM migr_script;
    xsltStylesheetPtr stylesheetptr = NULL;
    xmlDocPtr inputxml, transformedxml;
    FILE *outfile;
    gboolean migration_ok = FALSE;

    ENTER ();

    base_dir = g_strdup (g_get_home_dir ());
    for (iter = base_dir; *iter != 0; iter++)
    {
        if ( *iter == '\\')
            *iter = '/';
    }

    /* Only attempt to migrate if there is something to migrate */
    gconf_root    = g_build_filename(base_dir, ".gconf", NULL);
    gconf_apps    = g_build_filename(gconf_root, "apps", NULL);
    gconf_gnucash = g_build_filename(gconf_apps, "gnucash", NULL);
    migration_ok = (g_file_test (gconf_root, G_FILE_TEST_IS_DIR) &&
                    g_file_test (gconf_apps, G_FILE_TEST_IS_DIR) &&
                    g_file_test (gconf_gnucash, G_FILE_TEST_IS_DIR));
    g_free (gconf_root);
    g_free (gconf_apps);
    g_free (gconf_gnucash);
    if (!migration_ok)
    {
        g_free (base_dir);
        gnc_gsettings_set_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_MIGRATE_PREFS_DONE, TRUE);
        PINFO ("No pre-existing GConf gnucash section found.\n"
               "Most likely this system never ran GnuCash before.\n"
               "Assume migration is not needed.");
        LEAVE ();
        return;
    }

    pkgdatadir = gnc_path_get_pkgdatadir();
    stylesheet = g_build_filename(pkgdatadir, "make-prefs-migration-script.xsl", NULL);
    input      = g_build_filename(pkgdatadir, "migratable-prefs.xml", NULL);
    g_free (pkgdatadir);

    migration_ok = (g_file_test (stylesheet, G_FILE_TEST_IS_REGULAR) &&
                    g_file_test (input, G_FILE_TEST_IS_REGULAR));
    if (!migration_ok)
    {
        /* Critical files not found, abort migration */
        g_free (base_dir);
        g_free (stylesheet);
        g_free (input);
        PWARN ("Migration input file and stylesheet missing. Skip migration.");
        return;
    }

    command = g_strconcat ("(use-modules (migrate-prefs))(migration-prepare \"",
                           base_dir, "\")", NULL);
    DEBUG ("command = %s", command);
    migration_ok = scm_is_true (scm_eval_string (scm_from_utf8_string (command)));
    g_free (command);
    if (!migration_ok)
    {
        /* Preparation step failed */
        g_free (base_dir);
        g_free (stylesheet);
        g_free (input);
        PWARN ("Migration preparation step failed. Skip migration.");
        LEAVE ();
        return;
    }

    output  = g_build_filename(base_dir, ".gnc-migration-tmp", "migrate-prefs-user.scm", NULL);
    xmlSubstituteEntitiesDefault(1);
    xmlLoadExtDtdDefaultValue = 1;
    defaultEntityLoader = xmlGetExternalEntityLoader();
    xmlSetExternalEntityLoader(xsltprocExternalEntityLoader);
    stylesheetptr = xsltParseStylesheetFile((const xmlChar *)stylesheet);
    inputxml = xmlParseFile(input);
    transformedxml = xsltApplyStylesheet(stylesheetptr, inputxml, NULL);

    outfile = fopen(output, "w");
    xsltSaveResultToFile(outfile, transformedxml, stylesheetptr);
    fclose(outfile);

    xsltFreeStylesheet(stylesheetptr);
    xmlFreeDoc(inputxml);
    xmlFreeDoc(transformedxml);

    xsltCleanupGlobals();
    xmlCleanupParser();
    g_free (stylesheet);
    g_free (input);

    migr_script = scm_from_locale_string (output);
    scm_primitive_load (migr_script);
    g_free (output);

    migration_ok = scm_is_true (scm_c_eval_string ("(use-modules (migrate-prefs-user))(run-migration)"));
    if (!migration_ok)
    {
        /* Actual migration step failed */
        g_free (base_dir);
        PWARN ("Actual migration step failed. Skip migration.");
        LEAVE ();
        return;
    }

    /* If we got here, the preferences were migrated successfully
     * Mark this success in gsettings, so we won't run the migration again.
     */
    gnc_gsettings_set_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_MIGRATE_PREFS_DONE, TRUE);

    /* All that is left now is to cleanup... */
    command = g_strconcat ("(use-modules (migrate-prefs))(migration-cleanup \"",
                           base_dir, "\")", NULL);
    DEBUG ("command = %s", command);
    migration_ok = scm_is_true (scm_eval_string (scm_from_utf8_string (command)));
    g_free (command);
    if (!migration_ok) /* Cleanup step failed, not critical */
        PWARN ("Cleanup step failed. You may need to delete %s/.gnc-migration-tmp manually.", base_dir);
    else
        PINFO ("Preferences migration completed successfully");

    LEAVE ("");
    g_free (base_dir);

}


void gnc_gsettings_version_upgrade (void)
{
    /* Use versioning to ensure this routine will only sync once for each
     * superseded setting */
    int old_maj_min = gnc_gsettings_get_int (GNC_PREFS_GROUP_GENERAL, GNC_PREF_VERSION);
    int cur_maj_min = PROJECT_VERSION_MAJOR * 100 + PROJECT_VERSION_MINOR;

    /* Migrate preferences from gconf to gsettings */
    if (!gnc_gsettings_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_MIGRATE_PREFS_DONE))
        gnc_gsettings_migrate_from_gconf ();

    /* Convert settings to 3.0 compatibility level */
    if (old_maj_min < 207)
    {
        /* 'use-theme-colors' has been replaced with 'use-gnucash-color-theme'
         * which inverts the meaning of the setting */
        gboolean old_color_theme = gnc_gsettings_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_USE_THEME_COLORS);
        gnc_gsettings_set_bool (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_USE_GNUCASH_COLOR_THEME, !old_color_theme);
    }

    /* Only write current version if it's more recent than what was set */
    if (cur_maj_min > old_maj_min)
        gnc_gsettings_set_int (GNC_PREFS_GROUP_GENERAL, GNC_PREF_VERSION, cur_maj_min);
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
