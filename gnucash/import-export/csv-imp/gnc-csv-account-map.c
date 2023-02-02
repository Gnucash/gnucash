/*******************************************************************\
 * gnc-csv-account-map.c -- Load and Update Mappings                *
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
/** @file gnc-csv-account-map.c
    @brief Save and Load Mappings
    @author Copyright (c) 2015 Robert Fewell
*/
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "qof.h"
#include "gnc-ui-util.h"
#include "gnc-csv-account-map.h"

#define CSV_CATEGORY         "csv-account-map"

#define IMPORT_STRING        "String"
#define IMPORT_FULL_PATH     "FullPath"
#define IMPORT_ACCOUNT       "Account"

#define UNUSED_VAR     __attribute__ ((unused))

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule UNUSED_VAR log_module = G_LOG_DOMAIN;

/**************************************************
 * gnc_csv_account_map_search
 *
 * search the existing mappings for the account
 * linked to the import string.
 **************************************************/
Account * gnc_csv_account_map_search (const gchar *map_string)
{
    Account *root, *account = NULL;
    GList   *accts, *ptr;

    /* Get list of Accounts */
    root = gnc_book_get_root_account (gnc_get_current_book());
    accts = gnc_account_get_descendants_sorted (root);

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *tmp_acc = ptr->data;

        if (gnc_account_imap_find_account (tmp_acc, CSV_CATEGORY, map_string))
        {
            account = tmp_acc;
            break;
        }
    }
    g_list_free (accts);

    return account;
}


/**************************************************
 * gnc_csv_account_map_load_mappings
 *
 * load the existing mappings
 **************************************************/
void
gnc_csv_account_map_load_mappings (GtkTreeModel *mappings_store)
{
    GtkTreeIter iter;
    gboolean    valid;

    // Set iter to first entry of store
    valid = gtk_tree_model_get_iter_first (mappings_store, &iter);

    // Walk through the store trying to match to a map
    while (valid)
    {
        Account *account = NULL;
        gchar   *map_string;
        gchar   *fullpath;

        // Walk through the list, reading each row
        gtk_tree_model_get (GTK_TREE_MODEL(mappings_store), &iter, MAPPING_STRING, &map_string, MAPPING_ACCOUNT, &account, -1);

        // Look for an account matching the map_string
        // It may already be set in the tree model. If not we try to match the map_string with
        // - an entry in our saved account maps
        // - a full name of any of our existing accounts
        if (account ||
            (account = gnc_csv_account_map_search (map_string)) ||
            (account = gnc_account_lookup_by_full_name (gnc_get_current_root_account(), map_string)))
        {
            fullpath = gnc_account_get_full_name (account);
            gtk_list_store_set (GTK_LIST_STORE(mappings_store), &iter, MAPPING_FULLPATH, fullpath, -1);
            gtk_list_store_set (GTK_LIST_STORE(mappings_store), &iter, MAPPING_ACCOUNT, account, -1);
            g_free (fullpath);
        }

        g_free (map_string);
        valid = gtk_tree_model_iter_next (mappings_store, &iter);
    }
}


/**************************************************
 * gnc_csv_account_map_change_mappings
 *
 * change the existing mappings
 **************************************************/
void
gnc_csv_account_map_change_mappings (Account *old_account, Account *new_account, const gchar *map_string)
{
    if (strlen (map_string) == 0)
        return;

    if (old_account)
        gnc_account_imap_delete_account (old_account, CSV_CATEGORY, map_string);

    if (new_account)
	gnc_account_imap_add_account (new_account, CSV_CATEGORY, map_string, new_account);
}
