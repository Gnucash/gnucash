/*******************************************************************\
 * assistant-csv-export.h -- An assistant for exporting Accounts    *
 *                            and Transactions to a file            *
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
/** @file assistant-csv-export.h
    @brief CSV Export Assistant
    @author Copyright (c) 2012 Robert Fewell
*/
#ifndef GNC_ASSISTANT_CSV_EXPORT_H
#define GNC_ASSISTANT_CSV_EXPORT_H

#include "Account.h"

typedef enum
{
    XML_EXPORT_TREE,
    XML_EXPORT_TRANS
} CsvExportType;

typedef struct
{
    GtkWidget *table;
    GtkWidget *start_date_choose;
    GtkWidget *start_date_today;
    GtkWidget *start_date;
    GtkWidget *end_date_choose;
    GtkWidget *end_date_today;
    GtkWidget *end_date;

    time64     start_time;
    time64     end_time;
} CsvExportDate;

typedef struct
{
    GtkWidget        *acct_info;
    GtkWidget        *expense_radio;
    GtkWidget        *asset_radio;
    GtkWidget        *liab_eq_radio;
    GtkWidget        *account_treeview;
    GtkWidget        *select_button;
    GtkWidget        *num_acct_label;
    GList            *account_list;
    int               num_accounts;
    GNCAccountType    account_type;
} CsvExportAcc;


typedef struct
{
    CsvExportType export_type;
    CsvExportDate csvd;
    CsvExportAcc  csva;
    
    GtkWidget    *start_page;
    GtkWidget    *account_page;
    GtkWidget    *file_page;

    GtkWidget    *window;
    GtkWidget    *assistant;
    GtkWidget    *start_label;
    GtkWidget    *custom_entry;

    GtkWidget    *file_chooser;
    GtkWidget    *finish_label;
    GtkWidget    *summary_label;

    gchar        *starting_dir;
    gchar        *file_name;

    char         *separator_str;
    gboolean      use_quotes;
    gboolean      use_custom;
    gboolean      failed;
} CsvExportInfo;


/** The gnc_file_csv_export() will let the user export thte
 *  account tree or transactions to a delimited file.
 */
void gnc_file_csv_export (CsvExportType export_type);

#endif
