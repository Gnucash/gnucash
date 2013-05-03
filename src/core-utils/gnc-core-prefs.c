/*
 * gnc-core-prefs.c:
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
#include "config.h"
#include "gnc-core-prefs.h"
#include "gnc-version.h"
#include "libqof/qof/qof.h"

static gchar *namespace_regexp    = NULL;
static gboolean is_debugging      = FALSE;
static gboolean extras_enabled    = FALSE;
static gboolean use_compression   = TRUE; // This is also the default in GConf
static gint file_retention_policy = 1;    // 1 = "days", the default in GConf
static gint file_retention_days   = 30;   // This is also the default in GConf
static const gchar *gconf_path    = "/apps/gnucash";  // A sensible default

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

const gchar *
gnc_core_prefs_get_namespace_regexp(void)
{
    return namespace_regexp;
}

void
gnc_core_prefs_set_namespace_regexp(const gchar *str)
{
    if (namespace_regexp)
        g_free(namespace_regexp);

    if (str)
        namespace_regexp = g_strdup(str);
}

gboolean
gnc_core_prefs_is_debugging_enabled(void)
{
    return is_debugging;
}

void
gnc_core_prefs_set_debugging(gboolean d)
{
    is_debugging = d;
}

gboolean
gnc_core_prefs_is_extra_enabled(void)
{
    return extras_enabled;
}

void
gnc_core_prefs_set_extra(gboolean enabled)
{
    extras_enabled = enabled;
}

gboolean
gnc_core_prefs_get_file_save_compressed(void)
{
    return use_compression;
}

void
gnc_core_prefs_set_file_save_compressed(gboolean compressed)
{
    use_compression = compressed;
}

gint
gnc_core_prefs_get_file_retention_policy(void)
{
    return file_retention_policy;
}

void
gnc_core_prefs_set_file_retention_policy(gint policy)
{
    file_retention_policy = policy;
}

gint
gnc_core_prefs_get_file_retention_days(void)
{
    return file_retention_days;
}

void
gnc_core_prefs_set_file_retention_days(gint days)
{
    file_retention_days = days;
}

const gchar *
gnc_gconf_get_path_prefix (void)
{
    return gconf_path;
}

void
gnc_gconf_set_path_prefix (const gchar *path)
{
    gconf_path = path;
}

guint
gnc_core_prefs_get_long_version()
{
    return GNUCASH_MAJOR_VERSION * 1000000 +
           GNUCASH_MINOR_VERSION * 10000 +
           GNUCASH_MICRO_VERSION * 100 +
           GNUCASH_NANO_VERSION;
}
