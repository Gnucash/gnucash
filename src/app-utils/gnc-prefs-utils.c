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

#include "gnc-gsettings.h"
#include "gnc-prefs-utils.h"
#include "gnc-prefs.h"
#include "backend/xml/gnc-backend-xml.h"

static QofLogModule log_module = G_LOG_DOMAIN;

/* Keys used for core preferences */
#define GNC_PREF_FILE_COMPRESSION    "file-compression"
#define GNC_PREF_RETAIN_TYPE_NEVER   "retain-type-never"
#define GNC_PREF_RETAIN_TYPE_DAYS    "retain-type-days"
#define GNC_PREF_RETAIN_TYPE_FOREVER "retain-type-forever"
#define GNC_PREF_RETAIN_DAYS         "retain-days"

/***************************************************************
 * Initialization                                              *
 ***************************************************************/
static void
file_retain_changed_cb(gpointer gsettings, gchar *key, gpointer user_data)
{
    gint days = (int)gnc_prefs_get_float(GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_DAYS);
    gnc_prefs_set_file_retention_days (days);
}

static void
file_retain_type_changed_cb(gpointer gsettings, gchar *key, gpointer user_data)
{
    XMLFileRetentionType type = XML_RETAIN_ALL;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_TYPE_NEVER))
        type = XML_RETAIN_NONE;
    else if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_TYPE_DAYS))
        type = XML_RETAIN_DAYS;
    else if (!gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_TYPE_FOREVER))
        PWARN("no file retention policy was set, assuming conservative policy 'forever'");

    gnc_prefs_set_file_retention_policy (type);
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

    /* Check for invalid retain_type (days)/retain_days (0) combo.
     * This can happen either because a user changed the preferences
     * manually outside of GnuCash, or because the user upgraded from
     * gnucash version 2.3.15 or older. Back then, 0 retain_days meant
     * "keep forever". From 2.3.15 on this is controlled via a multiple
     * choice ("retain_type").
     * So if we find a 0 retain_days value with a "days" retain_type,
     * we will silently and conservatively interpret is as meaning
     * retain forever ("forever" retain_type).
     */
    if ( (gnc_prefs_get_file_retention_policy () == XML_RETAIN_DAYS) &&
            (gnc_prefs_get_file_retention_days () == 0 ) )
    {
        gnc_prefs_set_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_TYPE_FOREVER, TRUE);
        gnc_prefs_set_float (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_DAYS, 30);
        PWARN("retain 0 days policy was set, but this is probably not what the user wanted,\n"
              "assuming conservative policy 'forever'");
    }

    /* Add hooks to update core preferences whenever the associated preference changes */
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_DAYS,
                           file_retain_changed_cb, NULL);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_TYPE_NEVER,
                           file_retain_type_changed_cb, NULL);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_TYPE_DAYS,
                           file_retain_type_changed_cb, NULL);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_RETAIN_TYPE_FOREVER,
                           file_retain_type_changed_cb, NULL);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_FILE_COMPRESSION,
                           file_compression_changed_cb, NULL);

    /* Call the hooks once manually to initialize the core preferences */
    file_retain_changed_cb (NULL, NULL, NULL);
    file_retain_type_changed_cb (NULL, NULL, NULL);
    file_compression_changed_cb (NULL, NULL, NULL);
}
