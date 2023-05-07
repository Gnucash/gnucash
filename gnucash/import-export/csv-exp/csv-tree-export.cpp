/*******************************************************************\
 * csv-tree-export.cpp -- Export Account Tree to a file             *
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
/** @file csv-tree-export.c
    @brief CSV Export Account Tree
    @author Copyright (c) 2012 Robert Fewell
*/
#include <config.h>

#include <cstdio>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

#include <gnc-filepath-utils.h>
#include "gnc-commodity.h"
#include "gnc-ui-util.h"
#include "csv-tree-export.h"
#include "csv-export-helpers.hpp"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/*******************************************************
 * csv_tree_export
 *
 * write a list of accounts settings to a text file
 *******************************************************/
void
csv_tree_export (CsvExportInfo *info)
{
    ENTER("");
    DEBUG("File name is : %s", info->file_name);

    /* Open File for writing */
    auto ss{gnc_open_filestream(info->file_name)};

    /* Header string */
    StringVec headervec = {
        _("Type"), _("Full Account Name"), _("Account Name"),
        _("Account Code"), _("Description"), _("Account Color"),
        _("Notes"), _("Symbol"), _("Namespace"),
        _("Hidden"), _("Tax Info"), _("Placeholder")
    };

    /* Write header line */
    info->failed = ss.fail() ||
        !gnc_csv_add_line (ss, headervec, info->use_quotes, info->separator_str);

    /* Get list of Accounts */
    auto root{gnc_book_get_root_account (gnc_get_current_book())};
    auto accts{gnc_account_get_descendants_sorted (root)};
    auto str_or_empty = [](const char* a){ return a ? a : ""; };
    auto bool_to_char = [](bool b){ return b ? "T" : "F"; };

    /* Go through list of accounts */
    for (GList *ptr = accts; !info->failed && ptr; ptr = g_list_next (ptr))
    {
        auto acc{static_cast<Account*>(ptr->data)};
        DEBUG("Account being processed is : %s", xaccAccountGetName (acc));

        StringVec line = {
            xaccAccountTypeEnumAsString (xaccAccountGetType (acc)),
            account_get_fullname_str (acc),
            xaccAccountGetName (acc),
            str_or_empty (xaccAccountGetCode (acc)),
            str_or_empty (xaccAccountGetDescription (acc)),
            str_or_empty (xaccAccountGetColor (acc)),
            str_or_empty (xaccAccountGetNotes (acc)),
            gnc_commodity_get_mnemonic (xaccAccountGetCommodity (acc)),
            gnc_commodity_get_namespace (xaccAccountGetCommodity (acc)),
            bool_to_char (xaccAccountGetHidden (acc)),
            bool_to_char (xaccAccountGetTaxRelated (acc)),
            bool_to_char (xaccAccountGetPlaceholder (acc)),
        };
        info->failed = !gnc_csv_add_line (ss, line, info->use_quotes, info->separator_str);
    }

    g_list_free (accts);
    LEAVE("");
}



