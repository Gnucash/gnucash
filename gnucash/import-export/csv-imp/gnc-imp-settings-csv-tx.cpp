/*******************************************************************\
 * gnc-imp-settings-csv-tx.cpp -- Trans CSV Import Settings      *
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
/** @file gnc-imp-settings-csv-tx.cpp
    @brief CSV Import Settings
    @author Copyright (c) 2014 Robert Fewell
    @author Copyright (c) 2016 Geert Janssens
*/

#include "gnc-imp-settings-csv.hpp"
#include "gnc-imp-settings-csv-tx.hpp"
#include <sstream>

extern "C"
{
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "gnc-state.h"
#include "gnc-ui-util.h"
}

constexpr auto group_prefix = "Import csv,transaction - ";

#define CSV_COL_TYPES    "ColumnTypes"

#define CSV_ACCOUNT      "BaseAccount"
#define CSV_MULTI_SPLIT  "MultiSplit"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

preset_vec_trans presets_trans;

static std::shared_ptr<CsvTransImpSettings> create_int_no_preset(void)
{
    auto preset = std::make_shared<CsvTransImpSettings>();
    preset->m_name = get_no_settings();

    return preset;
}

static std::shared_ptr<CsvTransImpSettings> create_int_gnc_exp_preset(void)
{
    auto preset = std::make_shared<CsvTransImpSettings>();
    preset->m_name = get_gnc_exp();
    preset->m_skip_start_lines = 1;
    preset->m_multi_split = true;

    /* FIXME date and currency format should still be aligned with export format!
     * That's currently hard to do, because the export uses whatever the user
     * had set as global preference.
    preset->date_active = 0;
    preset->currency_active = 0;
    */
    preset->m_column_types = {
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
 * based on settings type.
 **************************************************/
const preset_vec_trans& get_import_presets_trans (void)
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
        auto gp = std::string {group_prefix};
        auto pos = group.find(gp);
        if (pos == std::string::npos)
            continue;

        preset_names.push_back(group.substr(gp.size()));
    }
    // string array from the state file is no longer needed now.
    g_strfreev (groups);

    /* We want our settings to appear sorted alphabetically to the user */
    std::sort(preset_names.begin(), preset_names.end());

    /* Now add each preset to our global list */
    presets_trans.clear();

    /* Start with the internally generated ones */
    presets_trans.push_back(create_int_no_preset());
    presets_trans.push_back(create_int_gnc_exp_preset());

    /* Then add all the ones we found in the state file */
    for (auto preset_name : preset_names)
    {
        auto preset = std::make_shared<CsvTransImpSettings>();
        preset->m_name = preset_name;
        preset->load();
        presets_trans.push_back(preset);
    }
    return presets_trans;
}

/**************************************************
 * load
 *
 * load the settings from a state key file
 **************************************************/
bool
CsvTransImpSettings::load (void)
{
    if (preset_is_reserved_name (m_name))
        return true;

    GError *key_error = nullptr;
    m_load_error = false;
    auto keyfile = gnc_state_get_current ();
    auto group = get_group_prefix() + m_name;

    // Start Loading the settings
    m_load_error = CsvImportSettings::load(); // load the common settings

    m_multi_split = g_key_file_get_boolean (keyfile, group.c_str(), CSV_MULTI_SPLIT, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    gchar *key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_ACCOUNT, &key_error);
    if (key_char && *key_char != '\0')
        m_base_account = gnc_account_lookup_by_full_name (gnc_get_current_root_account(), key_char);
    m_load_error |= handle_load_error (&key_error, group);
    if (key_char)
        g_free (key_char);

    gsize list_len;
    m_column_types.clear();
    gchar** col_types_str = g_key_file_get_string_list (keyfile, group.c_str(), CSV_COL_TYPES,
            &list_len, &key_error);
    for (uint32_t i = 0; i < list_len; i++)
    {
        auto col_types_it = std::find_if (gnc_csv_col_type_strs.begin(),
                gnc_csv_col_type_strs.end(), test_prop_type_str (col_types_str[i]));
        if (col_types_it != gnc_csv_col_type_strs.end())
        {
            /* Found a valid column type. Now check whether it is allowed
             * in the selected mode (two-split vs multi-split) */
            auto prop = sanitize_trans_prop (col_types_it->first, m_multi_split);
                m_column_types.push_back(prop);
            if (prop != col_types_it->first)
                PWARN("Found column type '%s', but this is blacklisted when multi-split mode is %s. "
                      "Inserting column type 'NONE' instead'.",
                        col_types_it->second, m_multi_split ? "enabled" : "disabled");
        }
        else
            PWARN("Found invalid column type '%s'. Inserting column type 'NONE' instead'.",
                    col_types_str[i]);
    }
    if (col_types_str)
        g_strfreev (col_types_str);

    return m_load_error;
}

/**************************************************
 * save
 *
 * save settings to a key file
 **************************************************/
bool
CsvTransImpSettings::save (void)
{
    if (preset_is_reserved_name (m_name))
    {
        PWARN ("Ignoring attempt to save to reserved name '%s'", m_name.c_str());
        return true;
    }

    if ((m_name.find('[') != std::string::npos))
    {
        PWARN ("Name '%s' contains invalid characters '[]'. Refusing to save", m_name.c_str());
        return true;
    }

    auto keyfile = gnc_state_get_current ();
    auto group = get_group_prefix() + m_name;

    // Drop previous saved settings with this name
    g_key_file_remove_group (keyfile, group.c_str(), nullptr);

    // Start Saving the settings
    bool error = CsvImportSettings::save(); // save the common settings

    if (error)
        return error;

    g_key_file_set_boolean (keyfile, group.c_str(), CSV_MULTI_SPLIT, m_multi_split);

    if (m_base_account)
        g_key_file_set_string (keyfile, group.c_str(), CSV_ACCOUNT, gnc_account_get_full_name(m_base_account));

    std::vector<const char*> col_types_str;
    for (auto col_type : m_column_types)
        col_types_str.push_back(gnc_csv_col_type_strs[col_type]);

    if (!col_types_str.empty())
        g_key_file_set_string_list (keyfile, group.c_str(), CSV_COL_TYPES,
                col_types_str.data(), col_types_str.size());

    return error;
}

void
CsvTransImpSettings::remove (void)
{
    if (preset_is_reserved_name (m_name))
        return;

    CsvImportSettings::remove();
}

const char*
CsvTransImpSettings::get_group_prefix (void)
{
    return group_prefix;
}
