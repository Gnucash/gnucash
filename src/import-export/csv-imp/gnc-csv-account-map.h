/*******************************************************************\
 * gnc-csv-account-map.h   -- Load and Update Mappings              *
 *                                                                  *
 * Copyright (C) 2015 Robert Fewell                                 *
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
/** @file gnc-csv-account-map.h
    @brief Save and Load Mappings
    @author Copyright (c) 2015 Robert Fewell
*/
#ifndef GNC_CSV_ACCOUNT_MAP_H
#define GNC_CSV_ACCOUNT_MAP_H

#include <gtk/gtk.h>
#include "Account.h"

/** Enumeration for the mappings liststore */
enum GncImportColumn {MAPPING_STRING, MAPPING_FULLPATH, MAPPING_ACCOUNT};

/** Load the import mappings.
 *
 */
void gnc_csv_account_map_load_mappings (GtkTreeModel *mappings_store);

/** Update the import mappings.
 *
 */
void gnc_csv_account_map_change_mappings (Account *old_account, Account *new_account, const gchar *map_string);

/** Returns a pointer to the account that matches the import string.
 *
 * @return A pointer to an account.
 */
Account * gnc_csv_account_map_search (const gchar *map_string);

#endif
