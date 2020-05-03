/*
 * gnc-prefs.c:
 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>
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

#include <stdlib.h>
#include <glib.h>
#include <config.h>
#include "gnc-prefs.h"
#include "gnc-prefs-p.h"
#include "gnc-version.h"

static gchar *namespace_regexp    = NULL;
static gboolean is_debugging      = FALSE;
static gboolean extras_enabled    = FALSE;
static gboolean use_compression   = TRUE; // This is also the default in the prefs backend
static gint file_retention_policy = 1;    // 1 = "days", the default in the prefs backend
static gint file_retention_days   = 30;   // This is also the default in the prefs backend

PrefsBackend *prefsbackend = NULL;

const gchar *
gnc_prefs_get_namespace_regexp(void)
{
    return namespace_regexp;
}

void
gnc_prefs_set_namespace_regexp(const gchar *str)
{
    if (namespace_regexp)
        g_free(namespace_regexp);

    if (str)
        namespace_regexp = g_strdup(str);
}

gboolean
gnc_prefs_is_debugging_enabled(void)
{
    return is_debugging;
}

void
gnc_prefs_set_debugging(gboolean d)
{
    is_debugging = d;
}

gboolean
gnc_prefs_is_extra_enabled(void)
{
    return extras_enabled;
}

void
gnc_prefs_set_extra(gboolean enabled)
{
    extras_enabled = enabled;
}

gboolean
gnc_prefs_get_file_save_compressed(void)
{
    return use_compression;
}

void
gnc_prefs_set_file_save_compressed(gboolean compressed)
{
    use_compression = compressed;
}

gint
gnc_prefs_get_file_retention_policy(void)
{
    return file_retention_policy;
}

void
gnc_prefs_set_file_retention_policy(gint policy)
{
    file_retention_policy = policy;
}

gint
gnc_prefs_get_file_retention_days(void)
{
    return file_retention_days;
}

void
gnc_prefs_set_file_retention_days(gint days)
{
    file_retention_days = days;
}

guint
gnc_prefs_get_long_version()
{
     return PROJECT_VERSION_MAJOR * 1000000 + PROJECT_VERSION_MINOR;
}

gulong gnc_prefs_register_cb (const char *group,
                              const gchar *pref_name,
                              gpointer func,
                              gpointer user_data)
{
    if (prefsbackend && prefsbackend->register_cb)
        return (prefsbackend->register_cb) (group, pref_name, func, user_data);
    else
        g_warning ("no backend loaded, or the backend doesn't define register_cb, returning 0");
        return 0;
}


void gnc_prefs_remove_cb_by_func (const gchar *group,
                                  const gchar *pref_name,
                                  gpointer func,
                                  gpointer user_data)
{
    if (prefsbackend && prefsbackend->remove_cb_by_func)
        (prefsbackend->remove_cb_by_func) (group, pref_name, func, user_data);
}


void gnc_prefs_remove_cb_by_id (const gchar *group,
                                guint id)
{
    if (prefsbackend && prefsbackend->remove_cb_by_id)
        (prefsbackend->remove_cb_by_id) (group, id);
}


guint gnc_prefs_register_group_cb (const gchar *group,
                                   gpointer func,
                                   gpointer user_data)
{
    if (prefsbackend && prefsbackend->register_group_cb)
        return (prefsbackend->register_group_cb) (group, func, user_data);
    else
        return 0;
}


void gnc_prefs_remove_group_cb_by_func (const gchar *group,
                                        gpointer func,
                                        gpointer user_data)
{
    if (prefsbackend && prefsbackend->remove_group_cb_by_func)
        (prefsbackend->remove_group_cb_by_func) (group, func, user_data);
}


void gnc_prefs_bind (const gchar *group,
                     /*@ null @*/ const gchar *pref_name,
                     gpointer object,
                     const gchar *property)
{
    if (prefsbackend && prefsbackend->bind)
        (prefsbackend->bind) (group, pref_name, object, property);
}


gboolean gnc_prefs_get_bool (const gchar *group,
                             /*@ null @*/ const gchar *pref_name)
{
    if (prefsbackend && prefsbackend->get_bool)
        return (prefsbackend->get_bool) (group, pref_name);
    else
        return FALSE;
}


gint gnc_prefs_get_int (const gchar *group,
                        const gchar *pref_name)
{
    if (prefsbackend && prefsbackend->get_int)
        return (prefsbackend->get_int) (group, pref_name);
    else
        return 0;
}


gint64 gnc_prefs_get_int64 (const gchar *group,
                            const gchar *pref_name)
{
    gint64 result = 0;
    GVariant *var = gnc_prefs_get_value(group, pref_name);
    result = g_variant_get_int64 (var);
    g_variant_unref (var);
    return result;
}


gdouble gnc_prefs_get_float (const gchar *group,
                             const gchar *pref_name)
{
    if (prefsbackend && prefsbackend->get_float)
        return (prefsbackend->get_float) (group, pref_name);
    else
        return 0.0;
}


gchar *gnc_prefs_get_string (const gchar *group,
                             const gchar *pref_name)
{
    if (prefsbackend && prefsbackend->get_string)
        return (prefsbackend->get_string) (group, pref_name);
    else
        return NULL;
}


gint gnc_prefs_get_enum (const gchar *group,
                         const gchar *pref_name)
{
    if (prefsbackend && prefsbackend->get_enum)
        return (prefsbackend->get_enum) (group, pref_name);
    else
        return 0;
}

void
gnc_prefs_get_coords (const gchar *group,
                      const gchar *pref_name,
                      gdouble *x, gdouble *y)
{
    GVariant *coords = gnc_prefs_get_value (group, pref_name);

    *x = 0;
    *y = 0;

    if (g_variant_is_of_type (coords, (const GVariantType *) "(dd)") )
        g_variant_get (coords, "(dd)", x, y);
    g_variant_unref (coords);
}


GVariant *gnc_prefs_get_value (const gchar *group,
                               const gchar *pref_name)
{
    if (prefsbackend && prefsbackend->get_value)
        return (prefsbackend->get_value) (group,pref_name);
    else
        return NULL;
}


gboolean gnc_prefs_set_bool (const gchar *group,
                             const gchar *pref_name,
                             gboolean value)
{
    if (prefsbackend && prefsbackend->set_bool)
        return (prefsbackend->set_bool) (group, pref_name, value);
    else
        return FALSE;
}


gboolean gnc_prefs_set_int (const gchar *group,
                            const gchar *pref_name,
                            gint value)
{
    if (prefsbackend && prefsbackend->set_int)
        return (prefsbackend->set_int) (group, pref_name, value);
    else
        return FALSE;
}


gboolean gnc_prefs_set_int64 (const gchar *group,
                              const gchar *pref_name,
                              gint64 value)
{
    GVariant *var = g_variant_new ("x",value);
    return gnc_prefs_set_value (group, pref_name, var);
}


gboolean gnc_prefs_set_float (const gchar *group,
                              const gchar *pref_name,
                              gdouble value)
{
    if (prefsbackend && prefsbackend->set_float)
        return (prefsbackend->set_float) (group, pref_name, value);
    else
        return FALSE;
}


gboolean gnc_prefs_set_string (const gchar *group,
                               const gchar *pref_name,
                               const gchar *value)
{
    if (prefsbackend && prefsbackend->set_string)
        return (prefsbackend->set_string) (group, pref_name, value);
    else
        return FALSE;
}


gboolean gnc_prefs_set_enum (const gchar *group,
                             const gchar *pref_name,
                             gint value)
{
    if (prefsbackend && prefsbackend->set_enum)
        return (prefsbackend->set_enum) (group, pref_name, value);
    else
        return FALSE;
}


gboolean gnc_prefs_set_coords (const gchar *group,
                               const gchar *pref_name,
                               gdouble x, gdouble y)
{
    GVariant *var = g_variant_new ("(dd)",x, y);
    return gnc_prefs_set_value (group, pref_name, var);
}


gboolean gnc_prefs_set_value (const gchar *group,
                              const gchar *pref_name,
                              GVariant *value)
{
    if (prefsbackend && prefsbackend->set_value)
        return (prefsbackend->set_value) (group, pref_name, value);
    else
        return FALSE;
}


void gnc_prefs_reset (const gchar *group,
                      const gchar *pref_name)
{
    if (prefsbackend && prefsbackend->reset)
        (prefsbackend->reset) (group, pref_name);
}

void gnc_prefs_reset_group (const gchar *group)
{
    if (prefsbackend && prefsbackend->reset_group)
        (prefsbackend->reset_group) (group);
}

gboolean gnc_prefs_is_set_up (void)
{
    return (prefsbackend !=NULL);
}

void gnc_prefs_block_all (void)
{
    if (prefsbackend && prefsbackend->block_all)
        (prefsbackend->block_all) ();
}

void gnc_prefs_unblock_all (void)
{
    if (prefsbackend && prefsbackend->unblock_all)
        (prefsbackend->unblock_all) ();
}
