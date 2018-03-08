/*******************************************************************\
 * gnc-imp-settings-csv.cpp -- Save and Load CSV Import Settings *
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
/** @file gnc-imp-settings-csv.cpp
    @brief CSV Import Settings
    @author Copyright (c) 2014 Robert Fewell
    @author Copyright (c) 2016 Geert Janssens
*/

#include "gnc-imp-settings-csv.hpp"
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

#define CSV_SEP          "Separators"

#define CSV_CUSTOM       "Custom"
#define CSV_CUSTOM_ENTRY "CustomEntry"

#define CSV_DATE         "DateFormat"
#define CSV_CURRENCY     "CurrencyFormat"

#define CSV_ENCODING     "Encoding"
#define CSV_COL_WIDTHS   "ColumnWidths"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

/**************************************************
 * handle_load_error
 *
 * record possible errors in the log file
 * ignore key-not-found errors though. We'll just
 * use a default value and go on.
 **************************************************/
bool
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

bool preset_is_reserved_name (const std::string& name)
{
    return ((name == no_settings) ||
            (name == _(no_settings.c_str())) ||
            (name == gnc_exp) ||
            (name == _(gnc_exp.c_str())));
}

std::string get_no_settings (void)
{
    return no_settings;
}

std::string get_gnc_exp (void)
{
    return gnc_exp;
}

/**************************************************
 * load_common
 *
 * load the settings from a state key file
 **************************************************/
bool
CsvImportSettings::load (void)
{
    GError *key_error = nullptr;
    m_load_error = false;
    auto group = get_group_prefix() + m_name;
    auto keyfile = gnc_state_get_current ();

    m_skip_start_lines = g_key_file_get_integer (keyfile, group.c_str(), CSV_SKIP_START, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    m_skip_end_lines = g_key_file_get_integer (keyfile, group.c_str(), CSV_SKIP_END, &key_error);
    m_load_error |= handle_load_error (&key_error, group);

    m_skip_alt_lines = g_key_file_get_boolean (keyfile, group.c_str(), CSV_SKIP_ALT, &key_error);
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

    // Widths
    gsize list_len;
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
 * save_common
 *
 * save settings to a key file
 **************************************************/
bool
CsvImportSettings::save (void)
{
    auto keyfile = gnc_state_get_current ();
    auto group = get_group_prefix() + m_name;

    // Start Saving the Common settings
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
    g_key_file_set_comment (keyfile, group.c_str(), CSV_DATE, cmt.c_str(), nullptr);
    g_key_file_set_integer (keyfile, group.c_str(), CSV_CURRENCY, m_currency_format);
    g_key_file_set_string (keyfile, group.c_str(), CSV_ENCODING, m_encoding.c_str());

    if (!m_column_widths.empty())
        g_key_file_set_integer_list (keyfile, group.c_str(), CSV_COL_WIDTHS,
                (gint*)(m_column_widths.data()), m_column_widths.size());

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
            g_warning ("Error reading group %s key %s: %s", group.c_str(), CSV_ENCODING, key_error->message);
            g_error_free (key_error);
        }
        else
            g_warning ("Error comparing group %s key %s: '%s' and '%s'", group.c_str(), CSV_ENCODING, enc_str.c_str(), group.c_str());
        error = true;
    }
    return error;
}

void
CsvImportSettings::remove (void)
{
    auto keyfile = gnc_state_get_current ();
    auto group = get_group_prefix() + m_name;
    g_key_file_remove_group (keyfile, group.c_str(), nullptr);
}
