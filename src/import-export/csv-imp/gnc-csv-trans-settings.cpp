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

const std::string csv_group_prefix{"CSV - "};
const std::string no_settings{N_("No Settings")};
const std::string gnc_exp{N_("GnuCash Export Format")};
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

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

preset_vec presets;

static std::shared_ptr<CsvTransSettings> create_int_no_preset(void)
{
    auto preset = std::make_shared<CsvTransSettings>();
    preset->name = no_settings;

    return preset;
}

static std::shared_ptr<CsvTransSettings> create_int_gnc_exp_preset(void)
{
    auto preset = std::make_shared<CsvTransSettings>();
    preset->name = gnc_exp;
    preset->header_rows = 1;
    preset->multi_split = true;

    preset->separator[SEP_COMMA] = true;

    /* FIXME date and currency format should still be aligned with export format!
     * That's currently hard to do, because the export uses whatever the user
     * had set as preference.
    preset->date_active = 0;
    preset->currency_active = 0;
    */
    preset->column_types = {
            GncTransPropType::DATE,
            GncTransPropType::UNIQUE_ID,
            GncTransPropType::NUM,
            GncTransPropType::DESCRIPTION,
            GncTransPropType::NOTES,
            GncTransPropType::COMMODITY,
            GncTransPropType::VOID_REASON,
            GncTransPropType::ACTION,
            GncTransPropType::MEMO,
            GncTransPropType::ACCOUNT,
            GncTransPropType::NONE,
            GncTransPropType::NONE,
            GncTransPropType::DEPOSIT,
            GncTransPropType::REC_STATE,
            GncTransPropType::REC_DATE,
            GncTransPropType::PRICE
    };

    return preset;
}

/**************************************************
 * find
 *
 * find all settings entries in the state key file
 **************************************************/
const preset_vec& get_trans_presets (void)
{

    // Search all Groups in the state key file for ones starting with prefix
    auto preset_names = std::vector<std::string>();
    auto keyfile = gnc_state_get_current ();
    gsize grouplength;
    gchar **groups = g_key_file_get_groups (keyfile, &grouplength);

    /* Start by building a sorted list of candidate presets as found in the state file */
    for (gsize i=0; i < grouplength; i++)
    {
        auto group = std::string(groups[i]);
        auto pos = group.find(csv_group_prefix);
        if (pos == std::string::npos)
            continue;

        preset_names.push_back(group.substr(csv_group_prefix.size()));
    }
    // string array from the state file is no longer needed now.
    g_strfreev (groups);

    /* We want our settings to appear sorted alphabetically to the user */
    std::sort(preset_names.begin(), preset_names.end());

    /* Now add each preset to our global list */
    presets.clear();

    /* Start with the internally generated ones */
    presets.push_back(create_int_no_preset());
    presets.push_back(create_int_gnc_exp_preset());

    /* Then add all the ones we found in the state file */
    for (auto preset_name : preset_names)
    {
        auto preset = std::make_shared<CsvTransSettings>();
        preset->name = preset_name;
        preset->load();
        presets.push_back(preset);
    }

    return presets;
}

bool trans_preset_is_reserved_name (const std::string& name)
{
    return ((name == no_settings) ||
            (name == _(no_settings.c_str())) ||
            (name == gnc_exp) ||
            (name == _(gnc_exp.c_str())));
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
CsvTransSettings::load (void)
{
    if (trans_preset_is_reserved_name (name))
        return true;

    GError *key_error = nullptr;
    load_error = false;
    auto group = csv_group_prefix + name;
    auto keyfile = gnc_state_get_current ();

    header_rows = g_key_file_get_integer (keyfile, group.c_str(), CSV_SKIP_START, &key_error);
    load_error |= handle_load_error (&key_error, group);

    footer_rows = g_key_file_get_integer (keyfile, group.c_str(), CSV_SKIP_END, &key_error);
    load_error |= handle_load_error (&key_error, group);

    skip_alt_rows = g_key_file_get_boolean (keyfile, group.c_str(), CSV_ALT_ROWS, &key_error);
    load_error |= handle_load_error (&key_error, group);

    multi_split = g_key_file_get_boolean (keyfile, group.c_str(), CSV_MULTI_SPLIT, &key_error);
    load_error |= handle_load_error (&key_error, group);

    csv_format = g_key_file_get_boolean (keyfile, group.c_str(), CSV_FORMAT, &key_error);
    if (key_error) csv_format = true; // default to true, but above command will return false in case of error
    load_error |= handle_load_error (&key_error, group);

    for (uint i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        gchar *sep;
        sep = g_strdup_printf ("%s%d", CSV_SEP, i);
        separator[i] = g_key_file_get_boolean (keyfile, group.c_str(), sep, &key_error);
        load_error |= handle_load_error (&key_error, group);
        g_free (sep);
    }

    custom = g_key_file_get_boolean (keyfile, group.c_str(), CSV_CUSTOM, &key_error);
    load_error |= handle_load_error (&key_error, group);

    gchar *key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_CUSTOM_ENTRY, &key_error);
    if (key_char && *key_char != '\0')
        custom_entry = key_char;
    load_error |= handle_load_error (&key_error, group);
    if (key_char)
        g_free (key_char);

    date_active = g_key_file_get_integer (keyfile, group.c_str(), CSV_DATE, &key_error);
    load_error |= handle_load_error (&key_error, group);

    currency_active = g_key_file_get_integer (keyfile, group.c_str(), CSV_CURRENCY, &key_error);
    load_error |= handle_load_error (&key_error, group);

    key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_ENCODING, &key_error);
    if (key_char && *key_char != '\0')
        encoding = key_char;
    else
        "UTF-8";
    load_error |= handle_load_error (&key_error, group);
    if (key_char)
        g_free (key_char);

    column_types.clear();
    gsize list_len;
    gchar** col_types_str = g_key_file_get_string_list (keyfile, group.c_str(), CSV_COL_TYPES,
            &list_len, &key_error);
    for (uint i = 0; i < list_len; i++)
    {
        auto col_types_it = std::find_if (gnc_csv_col_type_strs.begin(),
                gnc_csv_col_type_strs.end(), test_prop_type_str (col_types_str[i]));
        if (col_types_it != gnc_csv_col_type_strs.end())
        {
            /* Found a valid column type. Now check whether it is allowed
             * in the selected mode (two-split vs multi-split) */
            auto prop = sanitize_trans_prop (col_types_it->first, multi_split);
                column_types.push_back(prop);
            if (prop != col_types_it->first)
                PWARN("Found column type '%s', but this is blacklisted when multi-split mode is %s. "
                        "Inserting column type 'NONE' instead'.",
                        col_types_it->second, multi_split ? "enabled" : "disabled");
        }
    }
    if (col_types_str)
        g_strfreev (col_types_str);

    column_widths.clear();
    gint *col_widths_int = g_key_file_get_integer_list (keyfile, group.c_str(), CSV_COL_WIDTHS,
            &list_len, &key_error);
    for (uint i = 0; i < list_len; i++)
    {
        if (col_widths_int[i] > 0)
            column_widths.push_back(col_widths_int[i]);
    }
    load_error |= handle_load_error (&key_error, group);
    if (col_widths_int)
        g_free (col_widths_int);

    return load_error;
}


/**************************************************
 * save
 *
 * save settings to a key file
 **************************************************/
bool
CsvTransSettings::save (void)
{
    if (trans_preset_is_reserved_name (name))
    {
        PWARN ("Ignoring attempt to save to reserved name '%s'", name.c_str());
        return true;
    }

    auto keyfile = gnc_state_get_current ();
    auto group = csv_group_prefix + name;

    // Drop previous saved settings with this name
    g_key_file_remove_group (keyfile, group.c_str(), nullptr);

    // Start Saving the settings
    g_key_file_set_string (keyfile, group.c_str(), CSV_NAME, name.c_str());
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

    std::vector<const char*> col_types_str;
    for (auto col_type : column_types)
        col_types_str.push_back(gnc_csv_col_type_strs[col_type]);

    if (!col_types_str.empty())
        g_key_file_set_string_list (keyfile, group.c_str(), CSV_COL_TYPES,
                col_types_str.data(), col_types_str.size());

    if (!column_widths.empty())
        g_key_file_set_integer_list (keyfile, group.c_str(), CSV_COL_WIDTHS,
                (gint*)(column_widths.data()), column_widths.size());

    // Do a test read of encoding
    GError *key_error = nullptr;
    bool error = false;
    auto enc_val = g_key_file_get_string (keyfile, group.c_str(), CSV_ENCODING, &key_error);
    auto enc_str = std::string{enc_val};
    if (enc_val)
        g_free (enc_val);

    if ((key_error) || (enc_str != encoding.c_str()))
    {
        if (key_error)
        {
            g_warning ("Error reading group %s key %s: %s", group.c_str(), CSV_COL_TYPES, key_error->message);
            g_error_free (key_error);
        }
        else
            g_warning ("Error comparing group %s key %s: '%s' and '%s'", group.c_str(), CSV_COL_TYPES, enc_str.c_str(), group.c_str());
        error = true;
    }
    return error;
}

void
CsvTransSettings::remove (void)
{
    if (trans_preset_is_reserved_name (name))
        return;

    auto keyfile = gnc_state_get_current ();
    auto group = csv_group_prefix + name;
    g_key_file_remove_group (keyfile, group.c_str(), nullptr);
}


bool
CsvTransSettings::read_only (void)
{
    return ((name == no_settings) ||
            (name == _(no_settings.c_str())) ||
            (name == gnc_exp) ||
            (name == _(gnc_exp.c_str())));
}
