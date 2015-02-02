/*******************************************************************\
 * assistant-csv-fixed-trans-import.h -- An assistant for importing *
 *                                       set format transction file.*
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
/** @file assistant-csv-fixed-trans-import.h
    @brief CSV Import Assistant
    @author Copyright (c) 2011 Robert Fewell
*/
#ifndef GNC_ASSISTANT_CSV_FIXED_IMPORT_H
#define GNC_ASSISTANT_CSV_FIXED_IMPORT_H

// Account tree model
enum fixed_trans_import_model_columns
{
   FTDATE, FTTYPE, FTSDATE, FTACCT_NAME, FTNUMBER, FTDESCRIPTION, FTNOTES, FTMEMO, FTFULL_CAT_NAME,
   FTCAT_NAME, FTRTYPE, FTACTION, FTRECONCILE, FTAMOUNT_WITH_SYM, FTCOMMODITYM, FTCOMMODITYN,
   FTAMOUNT_NUM, FTRATE, FTROW_COLOR, FTN_COLUMNS
};

typedef struct
{
    GtkWidget       *window;
    GtkWidget       *assistant;

    GtkWidget       *file_chooser;           /**< The File Chooser widget on File page */
    GtkWidget       *tree_view;              /**< The Preview Treeview */
    GtkListStore    *store;                  /**< The Liststore of imported data */
    GString         *regexp;                 /**< The Regular expression string */
    GtkWidget       *header_row_spin;        /**< The Number of header rows widget */
    GtkComboBoxText *date_format_combo;      /**< The Combo Text widget for selecting the date format */
    GtkComboBoxText *currency_format_combo;  /**< The Combo Text widget for selecting the currency format */
    GtkWidget       *progressbar;            /**< The Progress widget on the Finish page */
    GtkWidget       *finish_label;           /**< The Label widget on the Finish page */
    GtkWidget       *summary_label;          /**< The Label widget on the Summary page */
    GtkWidget       *summary_error_view;     /**< The Text view widget on the Summary page */

    gchar           *starting_dir;           /**< The starting directory where import file is */
    gchar           *file_name;              /**< The File name to import */
    gchar           *error;                  /**< The Error Text */

    gint             header_rows;            /**< The Number of header rows, usually defaults to 1 */
    gint             num_new;                /**< The Number of new transactions imported */
    gint             num_duplicates;         /**< The Number of duplicate transactions not imported */
    gint             date_format;            /**< The format of the text in the date columns from date_format_internal. */
    gint             currency_format;        /**< The currency format, 0 for locale, 1 for comma dec and 2 for period */
    gboolean         new_book;               /**< Are we importing into a new book?; if yes, call book options */
} CsvFTImportInfo;


/** The gnc_file_csv_fixed_trans_import() will let the user import
 *  transactions from a delimited fixed format file.
 */
void gnc_file_csv_fixed_trans_import (void);
#endif
