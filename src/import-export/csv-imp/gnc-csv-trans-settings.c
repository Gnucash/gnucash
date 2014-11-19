/*******************************************************************\
 * gnc-csv-trans-settings.c -- Save and Load CSV Import Settings    *
 *                                                                  *
 * Copyright (C) 2014 Robert Fewell                                 *
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
\********************************************************************/
/** @file gnc-csv-trans-settings.c
    @brief CSV Import Settings
    @author Copyright (c) 2014 Robert Fewell
*/
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-state.h"
#include "gnc-csv-trans-settings.h"

#define CSV_GROUP_PREFIX "CSV - "
#define CSV_NAME         "Name"
#define CSV_FORMAT       "CsvFormat"
#define CSV_ALT_ROWS     "AltRows"
#define CSV_START_ROW    "StartRow"
#define CSV_END_ROWS     "EndRows"

#define CSV_SEP          "Separator"

#define CSV_CUSTOM       "Custom"
#define CSV_CUSTOM_ENTRY "CustomEntry"

#define CSV_DATE         "DateActive"
#define CSV_CURRENCY     "CurrencyActive"

#define CSV_ENCODING     "Encoding"
#define CSV_COL_TYPES    "ColumnTypes"
#define CSV_COL_WIDTHS   "ColumnWidths"


/**************************************************
 * gnc_csv_trans_new_settings_data
 *
 * Create CsvSettings structure and set defaults 
 **************************************************/
CsvSettings * gnc_csv_trans_new_settings_data (void)
{
    CsvSettings* settings_data = g_new (CsvSettings, 1);
    int i;

    settings_data->header_rows = 1;
    settings_data->skip_alt_rows = FALSE;
    settings_data->csv_format = TRUE;

    settings_data->encoding = "UTF-8";

    for (i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        settings_data->separator[i] = FALSE;
    }

    settings_data->custom = FALSE;
    settings_data->date_active = 0;
    settings_data->currency_active = 0;

    return settings_data;
}


/**************************************************
 * gnc_csv_trans_settings_data_free
 *
 * settings_data whose memory will be freed
 **************************************************/
void gnc_csv_trans_settings_data_free (CsvSettings* settings_data)
{
    /* All non-NULL pointers that have been initialized and must be freed. */
    g_free (settings_data);
}


/**************************************************
 * gnc_csv_trans_find_settings
 *
 * find the setting entries in a key file
 **************************************************/
void
gnc_csv_trans_find_settings (GtkTreeModel *settings_store)
{
    GtkTreeIter iter;
    GKeyFile   *keyfile;
    gchar     **groups = NULL;
    gint        i;
    gsize       grouplenght;
    GError     *key_error = NULL;

    // Get the Key file
    keyfile = gnc_state_get_current ();

    // Find all Groups
    groups = g_key_file_get_groups (keyfile, &grouplenght);

    // Clear the list store
    gtk_list_store_clear (GTK_LIST_STORE(settings_store));

    // Append the default entry
    gtk_list_store_append (GTK_LIST_STORE(settings_store), &iter);
    gtk_list_store_set (GTK_LIST_STORE(settings_store), &iter, SET_GROUP, NULL, SET_NAME, _("No Settings"), -1);

    // Search all Groups for ones starting with prefix
    for (i=0; i < grouplenght; i++)
    {
        if (g_str_has_prefix (groups[i], CSV_GROUP_PREFIX))
        {
            gchar *name = g_key_file_get_string (keyfile, groups[i], CSV_NAME, &key_error);

            if (key_error == NULL)
            {
                gtk_list_store_append (GTK_LIST_STORE(settings_store), &iter);
                gtk_list_store_set (GTK_LIST_STORE(settings_store), &iter, SET_GROUP, groups[i], SET_NAME, name, -1);
            }
            else
            {
                g_warning ("Error reading group '%s' name '%s': %s", groups[i], CSV_NAME, key_error->message);
                g_clear_error (&key_error);
            }
            g_free (name);
        }
    }
    // free the strings
    g_strfreev (groups);
}


/**************************************************
 * load_error
 *
 * record the error in the log file
 **************************************************/
static gboolean
load_error (GError **key_error, gchar *group)
{
    GError *kerror;
    kerror = g_error_copy (*key_error);
    g_warning ("Error reading group '%s' : %s", group, kerror->message);
    g_clear_error (key_error);
    g_error_free (kerror);
    return TRUE;
}

/**************************************************
 * gnc_csv_trans_load_settings
 *
 * load the settings from a key file
 **************************************************/
gboolean
gnc_csv_trans_load_settings (CsvSettings *settings_data, gchar *group)
{
    GKeyFile   *keyfile;
    gint        i;
    GError     *key_error = NULL;
    gboolean    key_boolean = FALSE;
    int         key_int = 0;
    gchar      *key_char = NULL;
    gboolean    error = FALSE;

    // Get the Key file
    keyfile = gnc_state_get_current ();

    key_int = g_key_file_get_integer (keyfile, group, CSV_START_ROW, &key_error);
    settings_data->header_rows = (key_error) ? 1 : key_int;
    if (key_error)
       error = load_error (&key_error, group);

    key_int = g_key_file_get_integer (keyfile, group, CSV_END_ROWS, &key_error);
    settings_data->footer_rows = (key_error) ? 0 : key_int;
    if (key_error)
       error = load_error (&key_error, group);

    key_boolean = g_key_file_get_boolean (keyfile, group, CSV_ALT_ROWS, &key_error);
    settings_data->skip_alt_rows = (key_error) ? FALSE : key_boolean;
    if (key_error)
       error = load_error (&key_error, group);

    key_boolean = g_key_file_get_boolean (keyfile, group, CSV_FORMAT, &key_error);
    settings_data->csv_format = (key_error) ? TRUE : key_boolean;
    if (key_error)
       error = load_error (&key_error, group);

    for (i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        gchar *sep;
        sep = g_strdup_printf ("%s%d", CSV_SEP, i);
        key_boolean = g_key_file_get_boolean (keyfile, group, sep, &key_error);
        settings_data->separator[i] = (key_error) ? FALSE : key_boolean;
        if (key_error)
           error = load_error (&key_error, group);
        g_free (sep);
    }

    key_boolean = g_key_file_get_boolean (keyfile, group, CSV_CUSTOM, &key_error);
    settings_data->custom = (key_error) ? FALSE : key_boolean;
    if (key_error)
       error = load_error (&key_error, group);

    settings_data->custom_entry = g_key_file_get_string (keyfile, group, CSV_CUSTOM_ENTRY, &key_error);
    if (key_error)
       error = load_error (&key_error, group);

    key_int = g_key_file_get_integer (keyfile, group, CSV_DATE, &key_error);
    settings_data->date_active = (key_error) ? 0 : key_int;
    if (key_error)
       error = load_error (&key_error, group);

    key_int = g_key_file_get_integer (keyfile, group, CSV_CURRENCY, &key_error);
    settings_data->currency_active = (key_error) ? 0 : key_int;
    if (key_error)
       error = load_error (&key_error, group);

    key_char = g_key_file_get_string (keyfile, group, CSV_ENCODING, &key_error);
    settings_data->encoding = (key_error) ? "UTF-8" : key_char;
    if (key_error)
       error = load_error (&key_error, group);

    settings_data->column_types = g_key_file_get_string (keyfile, group, CSV_COL_TYPES, &key_error);
    if (key_error)
       error = load_error (&key_error, group);

    settings_data->column_widths = g_key_file_get_string (keyfile, group, CSV_COL_WIDTHS, &key_error);
    if (key_error)
       error = load_error (&key_error, group);

    g_free (key_char);
    return error;
}


/**************************************************
 * gnc_csv_trans_save_settings
 *
 * save settings to a key file
 **************************************************/
gboolean
gnc_csv_trans_save_settings (CsvSettings *settings_data, gchar *settings_name)
{
    GKeyFile   *keyfile;
    gchar     **groups = NULL;
    gint        i;
    gsize       grouplenght;
    gchar      *group = NULL;
    gchar      *test_string = NULL;
    GError     *key_error = NULL;
    gboolean    error = FALSE;

    // Get the Key file
    keyfile = gnc_state_get_current ();

    // Find all Groups
    groups = g_key_file_get_groups (keyfile, &grouplenght);

    // Search all Groups for ones starting with prefix
    for (i=0; i < grouplenght; i++)
    {
        if (g_str_has_prefix (groups[i], CSV_GROUP_PREFIX))
        {
            gchar *name = g_key_file_get_string (keyfile, groups[i], CSV_NAME, NULL);

            if (g_strcmp0 (name, settings_name) == 0)
                group = g_strdup (groups[i]);

            g_free (name);
        }
    }

    // group is NULL, saving to a new group, create a guid 
    if (g_strcmp0 (group, NULL) == 0)
    {
        GncGUID *settings_guid;
        gchar *string_guid;

        settings_guid = guid_new ();
        string_guid = guid_to_string (settings_guid);

        group = g_strconcat (CSV_GROUP_PREFIX, string_guid, NULL);

        g_free (string_guid);
        guid_free (settings_guid);
    }

    // Start Saving the settings
    g_key_file_set_string (keyfile, group, CSV_NAME, settings_name);

    g_key_file_set_integer (keyfile, group, CSV_START_ROW, settings_data->header_rows);
    g_key_file_set_integer (keyfile, group, CSV_END_ROWS, settings_data->footer_rows);
    g_key_file_set_boolean (keyfile, group, CSV_ALT_ROWS, settings_data->skip_alt_rows);
    g_key_file_set_boolean (keyfile, group, CSV_FORMAT, settings_data->csv_format);

    for (i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        gchar *sep;
        sep = g_strdup_printf ("%s%d", CSV_SEP, i);
        g_key_file_set_boolean (keyfile, group, sep, settings_data->separator[i]);
        g_free (sep);
    }

    g_key_file_set_boolean (keyfile, group, CSV_CUSTOM, settings_data->custom);
    g_key_file_set_string (keyfile, group, CSV_CUSTOM_ENTRY, settings_data->custom_entry);

    g_key_file_set_integer (keyfile, group, CSV_DATE, settings_data->date_active);
    g_key_file_set_integer (keyfile, group, CSV_CURRENCY, settings_data->currency_active);

    g_key_file_set_string (keyfile, group, CSV_ENCODING, settings_data->encoding);

    g_key_file_set_string (keyfile, group, CSV_COL_TYPES, settings_data->column_types);

    g_key_file_set_string (keyfile, group, CSV_COL_WIDTHS, settings_data->column_widths);

    // free the strings
    g_free (settings_name);
    g_strfreev (groups);

    // Do a test read of column types
    test_string = g_key_file_get_string (keyfile, group, CSV_COL_TYPES, &key_error);

    if ((key_error) || (!g_strcmp0 (test_string, settings_data->column_types) == 0))
    {
        if (key_error)
        {
            g_warning ("Error reading group %s key %s: %s", group, CSV_COL_TYPES, key_error->message);
            g_error_free (key_error);
        }
        else
            g_warning ("Error comparing group %s key %s: '%s' and '%s'", group, CSV_COL_TYPES, test_string, group);
        error = TRUE;
    }
    g_free (group);
    g_free (test_string);
    return error;
}
