/********************************************************************\
 * gnc-prefs-utils.c -- utility functions for preferences management*
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

#include "gnc-gconf-utils.h"
#include "gnc-gsettings.h"
#include "gnc-prefs-utils.h"
#include "gnc-prefs.h"
#include "backend/xml/gnc-backend-xml.h"

static QofLogModule log_module = G_LOG_DOMAIN;

/* Keys used for core preferences */
#define GNC_PREF_FILE_COMPRESSION  "file_compression"
#define GNC_PREF_RETAIN_TYPE       "retain_type"
#define GNC_PREF_RETAIN_DAYS       "retain_days"

/***************************************************************
 * Initialization                                              *
 ***************************************************************/
static void
file_retain_changed_cb(GConfEntry *entry, gpointer user_data)
{
    gint days = (int)gnc_gconf_get_float(GCONF_GENERAL, KEY_RETAIN_DAYS, NULL);
    gnc_prefs_set_file_retention_days (days);
}

static void
file_retain_type_changed_cb(GConfEntry *entry, gpointer user_data)
{
    XMLFileRetentionType type;
    gchar *choice = gnc_gconf_get_string(GCONF_GENERAL, KEY_RETAIN_TYPE, NULL);
    if (!choice)
        choice = g_strdup("days");

    if (g_strcmp0 (choice, "never") == 0)
        type = XML_RETAIN_NONE;
    else if (g_strcmp0 (choice, "forever") == 0)
        type = XML_RETAIN_ALL;
    else
    {
        if (g_strcmp0 (choice, "days") != 0)
            PERR("bad value '%s'", choice ? choice : "(null)");
        type = XML_RETAIN_DAYS;
    }
    gnc_prefs_set_file_retention_policy (type);

    g_free (choice);
}

static void
file_compression_changed_cb(gpointer gsettings, gchar *key, gpointer user_data)
{
    gboolean file_compression = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_FILE_COMPRESSION);
    gnc_prefs_set_file_save_compressed (file_compression);
}


void gnc_prefs_init (void)
{
    gnc_gsettings_load_backend();

    /* Add hooks to update core preferences whenever the associated gconf key changes */
    gnc_gconf_general_register_cb(KEY_RETAIN_DAYS, file_retain_changed_cb, NULL);
    gnc_gconf_general_register_cb(KEY_RETAIN_TYPE, file_retain_type_changed_cb, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_FILE_COMPRESSION,
                              (GCallback) file_compression_changed_cb, NULL);

    /* Call the hooks once manually to initialize the core preferences */
    file_retain_changed_cb (NULL, NULL);
    file_retain_type_changed_cb (NULL, NULL);
    file_compression_changed_cb (NULL, NULL, NULL);

    /* Backwards compatibility code. Pre 2.3.15, 0 retain_days meant
     * "keep forever". From 2.3.15 on this is controlled via a multiple
     * choice ("retain_type"). So if we find a 0 retain_days value with
     * a "days" retain_type, we should interpret it as if we got a
     * "forever" retain_type.
     */
    if ( (gnc_prefs_get_file_retention_policy () == XML_RETAIN_DAYS) &&
            (gnc_prefs_get_file_retention_days () == 0 ) )
    {
        gnc_gconf_set_string (GCONF_GENERAL, KEY_RETAIN_TYPE, "forever", NULL);
    }
}
