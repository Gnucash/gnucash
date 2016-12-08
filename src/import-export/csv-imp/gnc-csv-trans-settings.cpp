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
#include "gnc-csv-trans-settings.hpp"

extern "C"
{
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-state.h"
}

#include <sstream>
#include <boost/tokenizer.hpp>

const std::string csv_group_prefix{"CSV - "};
const std::string no_settings{N_("No Settings")};
#define CSV_NAME         "Name"
#define CSV_FORMAT       "CsvFormat"
#define CSV_ALT_ROWS     "AltRows"
#define CSV_SKIP_START   "SkipStartRows"
#define CSV_SKIP_END     "SkipEndRows"
#define CSV_MULTI_SPLIT  "MultiSplit"

#define CSV_SEP          "Separator"

#define CSV_CUSTOM       "Custom"
#define CSV_CUSTOM_ENTRY "CustomEntry"

#define CSV_DATE         "DateActive"
#define CSV_CURRENCY     "CurrencyActive"

#define CSV_ENCODING     "Encoding"
#define CSV_COL_TYPES    "ColumnTypes"
#define CSV_COL_WIDTHS   "ColumnWidths"


/**************************************************
 * find
 *
 * find all settings entries in the state key file
 **************************************************/
void
CsvTransSettings::find (GtkTreeModel *settings_store)
{

    // Clear the list store
    gtk_list_store_clear (GTK_LIST_STORE(settings_store));

    // Append the default entry
    GtkTreeIter iter;
    gtk_list_store_append (GTK_LIST_STORE(settings_store), &iter);
    gtk_list_store_set (GTK_LIST_STORE(settings_store), &iter, SET_GROUP, NULL, SET_NAME, _(no_settings.c_str()), -1);

    // Search all Groups in the state key file for ones starting with prefix
    GKeyFile   *keyfile = gnc_state_get_current ();
    gsize grouplength;
    gchar **groups = g_key_file_get_groups (keyfile, &grouplength);

    for (gsize i=0; i < grouplength; i++)
    {
        if (g_str_has_prefix (groups[i], csv_group_prefix.c_str()))
        {
            GError *key_error = nullptr;
            gchar *name = g_key_file_get_string (keyfile, groups[i], CSV_NAME, &key_error);

            if (!key_error)
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
 * handle_load_error
 *
 * record possible errors in the log file
 * ignore key-not-found errors though. We'll just
 * use a default value and go on.
 **************************************************/
static bool
handle_load_error (GError **key_error, const std::string& group)
{
    if (!*key_error)
        return false;

    if ((*key_error)->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)
    {
        g_clear_error (key_error);
        return false;
    }

    g_warning ("Error reading group '%s' : %s", group.c_str(), (*key_error)->message);
    g_clear_error (key_error);
    return true;
}

/**************************************************
 * load
 *
 * load the settings from a state key file
 **************************************************/
bool
CsvTransSettings::load (const std::string& group)
{
    GError *key_error = nullptr;
    bool error = false;
    auto keyfile = gnc_state_get_current ();

    header_rows = g_key_file_get_integer (keyfile, group.c_str(), CSV_SKIP_START, &key_error);
    error |= handle_load_error (&key_error, group);

    footer_rows = g_key_file_get_integer (keyfile, group.c_str(), CSV_SKIP_END, &key_error);
    error |= handle_load_error (&key_error, group);

    skip_alt_rows = g_key_file_get_boolean (keyfile, group.c_str(), CSV_ALT_ROWS, &key_error);
    error |= handle_load_error (&key_error, group);

    multi_split = g_key_file_get_boolean (keyfile, group.c_str(), CSV_MULTI_SPLIT, &key_error);
    error |= handle_load_error (&key_error, group);

    csv_format = g_key_file_get_boolean (keyfile, group.c_str(), CSV_FORMAT, &key_error);
    if (key_error) csv_format = true; // default to true, but above command will return false in case of error
    error |= handle_load_error (&key_error, group);

    for (uint i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        gchar *sep;
        sep = g_strdup_printf ("%s%d", CSV_SEP, i);
        separator[i] = g_key_file_get_boolean (keyfile, group.c_str(), sep, &key_error);
        error |= handle_load_error (&key_error, group);
        g_free (sep);
    }

    custom = g_key_file_get_boolean (keyfile, group.c_str(), CSV_CUSTOM, &key_error);
    error |= handle_load_error (&key_error, group);

    gchar *key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_CUSTOM_ENTRY, &key_error);
    custom_entry = key_char;
    error |= handle_load_error (&key_error, group);
    if (key_char)
        g_free (key_char);

    date_active = g_key_file_get_integer (keyfile, group.c_str(), CSV_DATE, &key_error);
    error |= handle_load_error (&key_error, group);

    currency_active = g_key_file_get_integer (keyfile, group.c_str(), CSV_CURRENCY, &key_error);
    error |= handle_load_error (&key_error, group);

    key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_ENCODING, &key_error);
    encoding = (key_error) ? "UTF-8" : key_char;
    error |= handle_load_error (&key_error, group);
    if (key_char)
        g_free (key_char);

    key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_COL_TYPES, &key_error);
    error |= handle_load_error (&key_error, group);
    auto col_types_str = std::string { key_char };
    if (key_char)
        g_free (key_char);

    column_types.clear();
    if (!col_types_str.empty())
    {
        using Tokenizer = boost::tokenizer< boost::escaped_list_separator<char>>;
        boost::escaped_list_separator<char> sep("\\", ",", "\"");
        Tokenizer tok(col_types_str, sep);
        for (auto col_type_str : tok)
        {
            auto col_type = std::stoi(col_type_str);
            if (col_type >= static_cast<int>(GncTransPropType::NONE) &&
                col_type <= static_cast<int>(GncTransPropType::SPLIT_PROPS))
                column_types.push_back (static_cast<GncTransPropType>(col_type));
            else
                column_types.push_back (GncTransPropType::NONE);
        }
    }

    column_widths = g_key_file_get_string (keyfile, group.c_str(), CSV_COL_WIDTHS, &key_error);
    error |= handle_load_error (&key_error, group);

    return error;
}


/**************************************************
 * save
 *
 * save settings to a key file
 **************************************************/
bool
CsvTransSettings::save (const std::string& settings_name)
{
    auto keyfile = gnc_state_get_current ();
    std::string group = csv_group_prefix + settings_name;

    // Drop previous saved settings with this name
    g_key_file_remove_group (keyfile, group.c_str(), nullptr);

    // Start Saving the settings
    g_key_file_set_string (keyfile, group.c_str(), CSV_NAME, settings_name.c_str());
    g_key_file_set_boolean (keyfile, group.c_str(), CSV_MULTI_SPLIT, multi_split);
    g_key_file_set_integer (keyfile, group.c_str(), CSV_SKIP_START, header_rows);
    g_key_file_set_integer (keyfile, group.c_str(), CSV_SKIP_END, footer_rows);
    g_key_file_set_boolean (keyfile, group.c_str(), CSV_ALT_ROWS, skip_alt_rows);
    g_key_file_set_boolean (keyfile, group.c_str(), CSV_FORMAT, csv_format);

    for (guint i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        gchar *sep;
        sep = g_strdup_printf ("%s%d", CSV_SEP, i);
        g_key_file_set_boolean (keyfile, group.c_str(), sep, separator[i]);
        g_free (sep);
    }

    g_key_file_set_boolean (keyfile, group.c_str(), CSV_CUSTOM, custom);
    g_key_file_set_string (keyfile, group.c_str(), CSV_CUSTOM_ENTRY, custom_entry.c_str());
    g_key_file_set_integer (keyfile, group.c_str(), CSV_DATE, date_active);
    g_key_file_set_integer (keyfile, group.c_str(), CSV_CURRENCY, currency_active);
    g_key_file_set_string (keyfile, group.c_str(), CSV_ENCODING, encoding.c_str());

    std::stringstream ss;
    for (auto col_type : column_types)
    {
        if (!ss.str().empty())
            ss << ",";
        ss << static_cast<uint>(col_type);
    }

    g_key_file_set_string (keyfile, group.c_str(), CSV_COL_TYPES, ss.str().c_str());
    g_key_file_set_string (keyfile, group.c_str(), CSV_COL_WIDTHS, column_widths);

    // Do a test read of column types
    GError *key_error = nullptr;
    bool error = false;
    auto col_types_val = g_key_file_get_string (keyfile, group.c_str(), CSV_COL_TYPES, &key_error);
    auto test_string = std::string{col_types_val};
    g_free (col_types_val);

    if ((key_error) || (test_string != ss.str()))
    {
        if (key_error)
        {
            g_warning ("Error reading group %s key %s: %s", group.c_str(), CSV_COL_TYPES, key_error->message);
            g_error_free (key_error);
        }
        else
            g_warning ("Error comparing group %s key %s: '%s' and '%s'", group.c_str(), CSV_COL_TYPES, test_string.c_str(), group.c_str());
        error = true;
    }
    return error;
}
