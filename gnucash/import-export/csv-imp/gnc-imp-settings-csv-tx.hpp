/*******************************************************************\
 * gnc-imp-settings-csv-tx.hpp  -- Trans CSV Import Settings  *
 *                                                                  *
 * Copyright (C) 2017 Robert Fewell                                 *
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
/** @file gnc-imp-settings-csv-tx.hpp
    @brief CSV Import Settings
    @author Copyright (c) 2014 Robert Fewell
    @author Copyright (c) 2016 Geert Janssens
*/
#ifndef GNC_CSV_TRANS_IMPORT_SETTINGS_H
#define GNC_CSV_TRANS_IMPORT_SETTINGS_H

extern "C" {
#include <config.h>
#include "Account.h"
#include "gnc-commodity.h"
}

#include <string>
#include <vector>
#include "gnc-imp-props-tx.hpp"
#include "gnc-tokenizer.hpp"
#include "gnc-imp-settings-csv.hpp"

struct CsvTransImpSettings : public CsvImportSettings
{
    CsvTransImpSettings() : m_base_account {nullptr}, m_multi_split (false) { }

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

// Transaction Settings
Account      *m_base_account;                 // Base account
bool          m_multi_split;                  // Assume multiple lines per transaction
std::vector<GncTransPropType> m_column_types; // The Column types in order

protected:
    const char* get_group_prefix (void) override;
};

using preset_vec_trans = std::vector<std::shared_ptr<CsvTransImpSettings>>;

/** Creates a vector of CsvTransImpSettings which combines
 *  - one or more internally defined presets
 *  - all preset found in the state key file.
 *
 *  @return a reference to the populated vector.
 */
const preset_vec_trans& get_import_presets_trans (void);

#endif
