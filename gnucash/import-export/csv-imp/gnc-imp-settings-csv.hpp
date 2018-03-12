/*******************************************************************\
 * gnc-imp-settings-csv.hpp -- Save and Load CSV Import Settings *
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
/** @file gnc-imp-settings-csv.hpp
    @brief CSV Import Settings
    @author Copyright (c) 2014 Robert Fewell
    @author Copyright (c) 2016 Geert Janssens
*/
#ifndef GNC_CSV_IMPORT_SETTINGS_H
#define GNC_CSV_IMPORT_SETTINGS_H

extern "C" {
#include <config.h>
#include "Account.h"
#include "gnc-commodity.h"
}

#include <string>
#include <vector>
#include <boost/optional.hpp>
#include <gnc-datetime.hpp>
#include "gnc-tokenizer.hpp"

/** Enumeration for separator checkbutton types. These are the
 *  different types of checkbuttons that the user can click to
 *  configure separators in a delimited file. */
enum SEP_BUTTON_TYPES {SEP_SPACE, SEP_TAB, SEP_COMMA, SEP_COLON, SEP_SEMICOLON, SEP_HYPHEN,
                       SEP_NUM_OF_TYPES};

/** Enumeration for the settings combo's */
enum SETTINGS_COL {SET_GROUP, SET_NAME};

struct CsvImportSettings
{
    CsvImportSettings() : m_file_format (GncImpFileFormat::CSV), m_encoding {"UTF-8"},
            m_date_format {0}, m_currency_format {0},
            m_skip_start_lines{0}, m_skip_end_lines{0}, m_skip_alt_lines (false),
            m_separators {","}, m_load_error {false} { }

/** Save the gathered widget properties to a key File.
 *
 *  @return true if there was a problem in saving.
 */
bool save (void);

/** Load the widget properties from a key File.
 *
 *  @return true if there was a problem.
 */
bool load (void);

/** Remove the preset from the state file.
 */
void remove (void);

// Common Settings
std::string   m_name;                         // Name given to this preset by the user
GncImpFileFormat m_file_format;               // CSV import Format
std::string   m_encoding;                     // File encoding
int           m_date_format;                  // Date Active id
int           m_currency_format;              // Currency Active id
uint32_t      m_skip_start_lines;             // Number of header rows to skip
uint32_t      m_skip_end_lines;               // Number of footer rows to skip
bool          m_skip_alt_lines;               // Skip alternate rows
std::string   m_separators;                   // Separators for csv format
bool          m_load_error;                   // Was there an error while parsing the state file ?
std::vector<uint32_t> m_column_widths;        // The Column widths

protected:
    virtual const char* get_group_prefix (void) = 0;
};

std::string get_no_settings (void);
std::string get_gnc_exp (void);

/** Check whether name can be used as a preset name.
 *  The names of the internal presets are considered reserved.
 *  A preset with such a name should not be saved or deleted.
 */
bool preset_is_reserved_name (const std::string& name);

/**************************************************
 * handle_load_error
 *
 * record possible errors in the log file
 * ignore key-not-found errors though. We'll just
 * use a default value and go on.
 **************************************************/
bool
handle_load_error (GError **key_error, const std::string& group);

#endif
