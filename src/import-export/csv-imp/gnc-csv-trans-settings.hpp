/*******************************************************************\
 * gnc-csv-trans-settings.h   -- Save and Load CSV Import Settings  *
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
/** @file gnc-csv-trans-settings.h
    @brief CSV Import Settings
    @author Copyright (c) 2014 Robert Fewell
*/
#ifndef GNC_CSV_TRANS_SETTINGS_H
#define GNC_CSV_TRANS_SETTINGS_H

#include <string>
#include <vector>
#include "gnc-trans-props.hpp"

/** Enumeration for separator checkbutton types. These are the
 *  different types of checkbuttons that the user can click to
 *  configure separators in a delimited file. */
enum SEP_BUTTON_TYPES {SEP_SPACE, SEP_TAB, SEP_COMMA, SEP_COLON, SEP_SEMICOLON, SEP_HYPHEN,
                       SEP_NUM_OF_TYPES};

/** Enumeration for the settings combo's */
enum SETTINGS_COL {SET_GROUP, SET_NAME};

struct CsvTransSettings
{
    CsvTransSettings() : header_rows{0}, footer_rows{0}, csv_format (true),
                    skip_alt_rows (false), multi_split (false),
                    encoding {"UTF-8"}, custom {false}, custom_entry {""},
                    date_active {0}, currency_active {0}, load_error {false},
                    internal {false}
                    {
                        for (uint i = 0; i < SEP_NUM_OF_TYPES; i++)
                        {
                            separator[i] = false;
                        }
                        separator [SEP_COMMA] = true;
                    }

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

/** Check whether the user is allowed to save (over) or delete this preset or not.
 *  The internally generated presets are read-only. The others
 *  can be saved to the state file or deleted.
 *
 *  @param group The group name where the settings are stored in the
 *  key file.
 *
 *  @return true if there was a problem.
 */
bool read_only (void);


std::string   name;                         // Name given to this preset by the user
int           header_rows;                  // Number of header rows
int           footer_rows;                  // Number of footer rows
bool          csv_format;                   // CSV import Format
bool          skip_alt_rows;                // Skip alternate rows
bool          multi_split;                  // Assume multiple lines per transaction

std::string   encoding;                     // File encoding

bool          separator[SEP_NUM_OF_TYPES];  // The separators

bool          custom;                       // Custom entry set
std::string   custom_entry;                 // Custom Entry

int           date_active;                  // Date Active id
int           currency_active;              // Currency Active id
std::vector<GncTransPropType>  column_types;// The Column types in order
std::vector<uint> column_widths;            // The Column widths

bool          load_error;                   // Was there an error while parsing the state file ?
bool          internal;                     // true for internally generated presets
};

using preset_vec = std::vector<std::shared_ptr<CsvTransSettings>>;
/** Creates a vector of CsvTransSettings which combines
 *  - one or more internally defined presets
 *  - all preset found in the state key file.
 *
 *  @return a reference to the populated vector.
 */
const preset_vec& get_trans_presets (void);

/** Check whether name can be used as a preset name.
 *  The names of the internal presets are considered reserved.
 *  A preset with such a name should not be saved or deleted.
 */
bool trans_preset_is_reserved_name (const std::string& name);

#endif
