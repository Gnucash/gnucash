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

/** Enumeration for separator checkbutton types. These are the
 *  different types of checkbuttons that the user can click to
 *  configure separators in a delimited file. */
enum SEP_BUTTON_TYPES {SEP_SPACE, SEP_TAB, SEP_COMMA, SEP_COLON, SEP_SEMICOLON, SEP_HYPHEN,
                       SEP_NUM_OF_TYPES};

/** Enumeration for the settings combo's */
enum SETTINGS_COL {SET_GROUP, SET_NAME};

typedef struct
{
    int           header_rows;                  // Number of header rows
    int           footer_rows;                  // Number of footer rows
    gboolean      csv_format;                   // CSV import Format
    gboolean      skip_alt_rows;                // Skip alternate rows

    const gchar  *encoding;                     // File encoding

    gboolean      separator[SEP_NUM_OF_TYPES];  // The seperators

    gboolean      custom;                       // Custom entry set
    const gchar  *custom_entry;                 // Custom Entry

    int           date_active;                  // Date Active id
    int           currency_active;              // Currency Active id
    const gchar  *column_types;                 // The Column types in order
    const gchar  *column_widths;                // The Column widths
} CsvSettings;

/** Finds CSV settings entries in the key file and populates the
 *  tree model.
 *
 *  @param settings_store The liststore that is used for the combo's
 *  which holds the key name and visual text.
 */
void gnc_csv_trans_find_settings (GtkTreeModel *settings_store);

/** Save the gathered widget properties to a key File.
 *
 *  @param settings_data The settings structure where all the settings
 *  are located.
 *
 *  @param settings_name The name the settings will be stored under.
 *
 *  @return TRUE if there was a problem in saving.
 */
gboolean gnc_csv_trans_save_settings (CsvSettings *settings_data, gchar *settings_name);

/** Load the widget properties from a key File.
 *
 *  @param settings_data The settings structure where all the settings
 *  are located.
 *
 *  @param group The group name where the settings are stored in the
 *  key file.
 *
 *  @return TRUE if there was a problem.
 */
gboolean gnc_csv_trans_load_settings (CsvSettings *settings_data, gchar *group);

/** Create a new CsvSettings structure and set default values.
 *
 *  @return CsvSettings settings structure.
 */
CsvSettings * gnc_csv_trans_new_settings_data (void);

/** Free the CsvSettings structure.
 */
void gnc_csv_trans_settings_data_free (CsvSettings *settings_data);

#endif
