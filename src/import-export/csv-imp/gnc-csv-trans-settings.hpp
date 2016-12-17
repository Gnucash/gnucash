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

extern "C" {
#include "config.h"
#include "Account.h"
}

#include <string>
#include <vector>
#include "gnc-trans-props.hpp"
#include "gnc-tokenizer.hpp"

/** Enumeration for separator checkbutton types. These are the
 *  different types of checkbuttons that the user can click to
 *  configure separators in a delimited file. */
enum SEP_BUTTON_TYPES {SEP_SPACE, SEP_TAB, SEP_COMMA, SEP_COLON, SEP_SEMICOLON, SEP_HYPHEN,
                       SEP_NUM_OF_TYPES};

/** Enumeration for the settings combo's */
enum SETTINGS_COL {SET_GROUP, SET_NAME};

struct CsvTransSettings
{
    CsvTransSettings() : file_format (GncImpFileFormat::CSV), encoding {"UTF-8"},
            multi_split (false), date_format {0}, currency_format {0},
            skip_start_lines{0}, skip_end_lines{0}, skip_alt_lines (false),
            separators {","}, base_account {nullptr},
            load_error {false}, internal {false} { }

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
GncImpFileFormat file_format;               // CSV import Format
std::string   encoding;                     // File encoding
bool          multi_split;                  // Assume multiple lines per transaction
int           date_format;                  // Date Active id
int           currency_format;              // Currency Active id
uint           skip_start_lines;             // Number of header rows to skip
uint           skip_end_lines;               // Number of footer rows to skip
bool          skip_alt_lines;                // Skip alternate rows
std::string   separators;                   // Separators for csv format

Account      *base_account;                 // Base account
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
