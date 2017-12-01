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
    @author Copyright (c) 2016 Geert Janssens
*/

#include "gnc-csv-trans-settings.hpp"
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

const std::string csv_group_prefix{"CSV-"};
const std::string no_settings{N_("No Settings")};
const std::string gnc_exp{N_("GnuCash Export Format")};
#define CSV_NAME         "Name"
#define CSV_FORMAT       "CsvFormat"
#define CSV_SKIP_ALT     "SkipAltLines"
#define CSV_SKIP_START   "SkipStartLines"
#define CSV_SKIP_END     "SkipEndLines"
#define CSV_MULTI_SPLIT  "MultiSplit"

#define CSV_SEP          "Separators"

#define CSV_CUSTOM       "Custom"
#define CSV_CUSTOM_ENTRY "CustomEntry"

#define CSV_DATE         "DateFormat"
#define CSV_CURRENCY     "CurrencyFormat"

#define CSV_ENCODING     "Encoding"
#define CSV_COL_TYPES    "ColumnTypes"
#define CSV_COL_WIDTHS   "ColumnWidths"
#define CSV_ACCOUNT      "BaseAccount"
#define CSV_TO_CURR      "PriceToCurrency"
#define CSV_FROM_COMM    "PriceFromCommodity"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

preset_vec presets;

static std::shared_ptr<CsvTransSettings> create_int_no_preset(const std::string& set_type)
{
    auto preset = std::make_shared<CsvTransSettings>();
    preset->m_name = no_settings;
    preset->m_settings_type = set_type;

    return preset;
}

static std::shared_ptr<CsvTransSettings> create_int_gnc_exp_preset(void)
{
    auto preset = std::make_shared<CsvTransSettings>();
    preset->m_name = gnc_exp;
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
const preset_vec& get_trans_presets (const std::string& set_type)
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
        auto gp = csv_group_prefix + set_type + " - ";
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
    presets.clear();

    /* Start with the internally generated ones */
    presets.push_back(create_int_no_preset(set_type));

    if (set_type.compare("TRANS") == 0)
        presets.push_back(create_int_gnc_exp_preset());

    /* Then add all the ones we found in the state file */
    for (auto preset_name : preset_names)
    {
        auto preset = std::make_shared<CsvTransSettings>();
        preset->m_settings_type = set_type;
        preset->m_name = preset_name;
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
    if (trans_preset_is_reserved_name (m_name))
        return true;

    GError *key_error = nullptr;
    m_load_error = false;
    auto group = csv_group_prefix + m_settings_type + " - " + m_name;
    auto keyfile = gnc_state_get_current ();

    m_skip_start_lines = g_key_file_get_integer (keyfile, group.c_str(), CSV_SKIP_START, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    m_skip_end_lines = g_key_file_get_integer (keyfile, group.c_str(), CSV_SKIP_END, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    m_skip_alt_lines = g_key_file_get_boolean (keyfile, group.c_str(), CSV_SKIP_ALT, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    m_multi_split = g_key_file_get_boolean (keyfile, group.c_str(), CSV_MULTI_SPLIT, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    auto csv_format = g_key_file_get_boolean (keyfile, group.c_str(), CSV_FORMAT, &key_error);
    if (key_error) csv_format = true; // default to true, but above command will return false in case of error
    m_load_error |= handle_load_error (&key_error, group);
    if (csv_format)
        m_file_format = GncImpFileFormat::CSV;
    else
        m_file_format = GncImpFileFormat::FIXED_WIDTH;

    gchar *key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_SEP, &key_error);
    if (key_char && *key_char != '\0')
        m_separators = key_char;
    m_load_error |= handle_load_error (&key_error, group);
    if (key_char)
        g_free (key_char);

    m_date_format = g_key_file_get_integer (keyfile, group.c_str(), CSV_DATE, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    m_currency_format = g_key_file_get_integer (keyfile, group.c_str(), CSV_CURRENCY, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_ENCODING, &key_error);
    if (key_char && *key_char != '\0')
        m_encoding = key_char;
    else
        m_encoding = "UTF-8";
    m_load_error |= handle_load_error (&key_error, group);
    if (key_char)
        g_free (key_char);

    gsize list_len;

    // Transactions
    if (m_settings_type.compare("TRANS") == 0)
    {
        key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_ACCOUNT, &key_error);
        if (key_char && *key_char != '\0')
            m_base_account = gnc_account_lookup_by_full_name (gnc_get_current_root_account(), key_char);
        m_load_error |= handle_load_error (&key_error, group);
        if (key_char)
            g_free (key_char);

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
    }

    // Price
    if (m_settings_type.compare("PRICE") == 0)
    {
        key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_TO_CURR, &key_error);
        if (key_char && *key_char != '\0')
            m_to_currency = parse_commodity_price_comm (key_char);
        m_load_error |= handle_load_error (&key_error, group);
        if (key_char)
            g_free (key_char);

        key_char = g_key_file_get_string (keyfile, group.c_str(), CSV_FROM_COMM, &key_error);
        if (key_char && *key_char != '\0')
            m_from_commodity = parse_commodity_price_comm (key_char);
        m_load_error |= handle_load_error (&key_error, group);
        if (key_char)
            g_free (key_char);

        m_column_types.clear();
        gchar** col_types_str_price = g_key_file_get_string_list (keyfile, group.c_str(), CSV_COL_TYPES,
                &list_len, &key_error);
        for (uint32_t i = 0; i < list_len; i++)
        {
            auto col_types_it = std::find_if (gnc_price_col_type_strs.begin(),
                    gnc_price_col_type_strs.end(), test_price_prop_type_str (col_types_str_price[i]));
            if (col_types_it != gnc_price_col_type_strs.end())
            {
                // Found a valid column type
                m_column_types_price.push_back(col_types_it->first);
            }
            else
                PWARN("Found invalid column type '%s'. Inserting column type 'NONE' instead'.",
                        col_types_str_price[i]);
        }
        if (col_types_str_price)
            g_strfreev (col_types_str_price);
    }

    // Widths
    m_column_widths.clear();
    gint *col_widths_int = g_key_file_get_integer_list (keyfile, group.c_str(), CSV_COL_WIDTHS,
            &list_len, &key_error);
    for (uint32_t i = 0; i < list_len; i++)
    {
        if (col_widths_int[i] > 0)
            m_column_widths.push_back(col_widths_int[i]);
    }
    m_load_error |= handle_load_error (&key_error, group);
    if (col_widths_int)
        g_free (col_widths_int);

    return m_load_error;
}


/**************************************************
 * save
 *
 * save settings to a key file
 **************************************************/
bool
CsvTransSettings::save (void)
{
    if (trans_preset_is_reserved_name (m_name))
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
    auto group = csv_group_prefix + m_settings_type + " - " + m_name;

    // Drop previous saved settings with this name
    g_key_file_remove_group (keyfile, group.c_str(), nullptr);

    // Start Saving the settings
    // Common
    g_key_file_set_string (keyfile, group.c_str(), CSV_NAME, m_name.c_str());

    g_key_file_set_integer (keyfile, group.c_str(), CSV_SKIP_START, m_skip_start_lines);
    g_key_file_set_integer (keyfile, group.c_str(), CSV_SKIP_END, m_skip_end_lines);
    g_key_file_set_boolean (keyfile, group.c_str(), CSV_SKIP_ALT, m_skip_alt_lines);
    g_key_file_set_boolean (keyfile, group.c_str(), CSV_FORMAT,
        (m_file_format == GncImpFileFormat::CSV) ? true : false);

    g_key_file_set_string (keyfile, group.c_str(), CSV_SEP, m_separators.c_str());
    g_key_file_set_integer (keyfile, group.c_str(), CSV_DATE, m_date_format);
    std::ostringstream cmt_ss;
    cmt_ss << "Supported date formats: ";
    int fmt_num = 0;
    std::for_each (GncDate::c_formats.cbegin(), GncDate::c_formats.cend(),
                    [&cmt_ss, &fmt_num](const GncDateFormat& fmt)
                        { cmt_ss << fmt_num++ << ": '" << fmt.m_fmt << "', "; });
    auto cmt = cmt_ss.str().substr(0, static_cast<long>(cmt_ss.tellp()) - 2);
    g_key_file_set_comment (keyfile, group.c_str(), CSV_DATE,
                            cmt.c_str(), nullptr);
    g_key_file_set_integer (keyfile, group.c_str(), CSV_CURRENCY, m_currency_format);
    g_key_file_set_string (keyfile, group.c_str(), CSV_ENCODING, m_encoding.c_str());

    if (!m_column_widths.empty())
        g_key_file_set_integer_list (keyfile, group.c_str(), CSV_COL_WIDTHS,
                (gint*)(m_column_widths.data()), m_column_widths.size());

    // Transaction
    if (m_settings_type.compare("TRANS") == 0)
    {
        g_key_file_set_boolean (keyfile, group.c_str(), CSV_MULTI_SPLIT, m_multi_split);

        if (m_base_account)
            g_key_file_set_string (keyfile, group.c_str(), CSV_ACCOUNT, gnc_account_get_full_name(m_base_account));

        std::vector<const char*> col_types_str;
        for (auto col_type : m_column_types)
            col_types_str.push_back(gnc_csv_col_type_strs[col_type]);

        if (!col_types_str.empty())
            g_key_file_set_string_list (keyfile, group.c_str(), CSV_COL_TYPES,
                    col_types_str.data(), col_types_str.size());
    }

    // Price
    if (m_settings_type.compare("PRICE") == 0)
    {
        if (m_to_currency)
        {
            auto unique_name = g_strconcat (gnc_commodity_get_namespace (m_to_currency), "::",
                               gnc_commodity_get_mnemonic (m_to_currency), nullptr);
            g_key_file_set_string (keyfile, group.c_str(), CSV_TO_CURR, unique_name);
            g_free (unique_name);
        }

        if (m_from_commodity)
        {
            auto unique_name = g_strconcat (gnc_commodity_get_namespace (m_from_commodity), "::",
                               gnc_commodity_get_mnemonic (m_from_commodity), nullptr);
            g_key_file_set_string (keyfile, group.c_str(), CSV_FROM_COMM, unique_name);
            g_free (unique_name);
        }

        std::vector<const char*> col_types_str_price;
        for (auto col_type : m_column_types_price)
            col_types_str_price.push_back(gnc_price_col_type_strs[col_type]);

        if (!col_types_str_price.empty())
            g_key_file_set_string_list (keyfile, group.c_str(), CSV_COL_TYPES,
                    col_types_str_price.data(), col_types_str_price.size());
    }

    // Do a test read of encoding
    GError *key_error = nullptr;
    bool error = false;
    auto enc_val = g_key_file_get_string (keyfile, group.c_str(), CSV_ENCODING, &key_error);
    auto enc_str = std::string{enc_val};
    if (enc_val)
        g_free (enc_val);

    if ((key_error) || (enc_str != m_encoding.c_str()))
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
    if (trans_preset_is_reserved_name (m_name))
        return;

    auto keyfile = gnc_state_get_current ();
    auto group = csv_group_prefix + m_settings_type + " - " + m_name;
    g_key_file_remove_group (keyfile, group.c_str(), nullptr);
}


bool
CsvTransSettings::read_only (void)
{
    return ((m_name == no_settings) ||
            (m_name == _(no_settings.c_str())) ||
            (m_name == gnc_exp) ||
            (m_name == _(gnc_exp.c_str())));
}
