/*******************************************************************\
 * csv-account-import.h -- Account importing from file              *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
 *                                                                  *
 * Based on code from bi_import written by Sebastian Held  and      *
 * Mike Evans.                                                      *
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

#ifndef CSV_ACCOUNT_IMPORT_H
#define CSV_ACCOUNT_IMPORT_H

#include <glib.h>
#include <gtk/gtk.h>

#include "assistant-csv-account-import.h"

enum _csv_import_result
{
    RESULT_OK,
    RESULT_OPEN_FAILED,
    RESULT_ERROR_IN_REGEXP,
    MATCH_FOUND,
};
typedef enum _csv_import_result csv_import_result;

csv_import_result
csv_import_read_file (const gchar *filename, const gchar *parser_regexp, GtkListStore *store, guint max_rows );

void csv_account_import (CsvImportInfo *info);

#endif /* CSV_ACCOUNT_IMPORT_H */

