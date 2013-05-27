/*******************************************************************\
 * assistant-csv-account-import.h -- An assistant for importing     *
 *                                         Accounts from a file.    *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
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
/** @file assistant-csv-account-import.h
    @brief CSV Import Assistant
    @author Copyright (c) 2011 Robert Fewell
*/
#ifndef GNC_ASSISTANT_CSV_IMPORT_H
#define GNC_ASSISTANT_CSV_IMPORT_H


// Account tree model
enum tree_import_model_columns
{
    TYPE, FULL_NAME, NAME, CODE, DESCRIPTION, COLOR,
    NOTES, COMMODITYM, COMMODITYN, HIDDEN, TAX, PLACE_HOLDER, ROW_COLOR,
    N_COLUMNS
};

typedef struct
{
    GtkWidget    *window;
    GtkWidget    *assistant;

    GtkWidget    *file_chooser;
    GtkWidget    *tree_view;
    GtkListStore *store;
    GString      *regexp;
    GtkWidget    *header_row_spin;
    GtkWidget    *finish_label;
    GtkWidget    *summary_label;
    GtkWidget    *summary_error_view;

    gchar        *starting_dir;
    gchar        *file_name;
    gchar        *error;

    int           header_rows;
    int           num_new;
    int           num_updates;
    gboolean      new_book; /**< Are we importing into a new book?; if yes, call book options */
} CsvImportInfo;


/** The gnc_file_csv_account_import() will let the user import
 *  accounts from a delimited file.
 */
void gnc_file_csv_account_import (void);
#endif
