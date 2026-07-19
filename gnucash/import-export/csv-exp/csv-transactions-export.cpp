/*******************************************************************\
 * csv-actions-export.c -- Export Transactions to a file       *
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
/** @file csv-transactions-export.c
    @brief CSV Export Transactions
    @author Copyright (c) 2012 Robert Fewell
*/
#include "config.h"

#include <glib/gstdio.h>
#include <stdbool.h>

#include <string>
#include <unordered_set>

#include <gnc-filepath-utils.h>
#include "gnc-commodity.h"
#include "gnc-ui-util.h"
#include "Query.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "qofbookslots.h"
#include "guid.hpp"

#include "csv-transactions-export.h"
#include "csv-transaction-export-line.hpp"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/******************************************************************************/

using TransSet = std::unordered_set<Transaction *>;

/*******************************************************
 * account_splits
 *
 * gather the splits / transactions for an account and
 * send them to a file
 *******************************************************/
static void
export_query_splits(CsvExportInfo *info, bool is_trading_acct,
                    std::ofstream &ss, TransSet &trans_set)
{
    g_return_if_fail(info);

    /* Run the query */
    for (GList *splits = qof_query_run(info->query); /*!info->failed && */ splits;
         splits = splits->next)
    {
        auto split{static_cast<Split *>(splits->data)};
        auto trans{xaccSplitGetParent(split)};

        // Look for trans already exported in trans_set
        if (!trans_set.emplace(trans).second)
        {
            continue;
        }

        // Look for blank split
        Account *split_acc = xaccSplitGetAccount(split);
        if (!split_acc)
        {
            continue;
        }

        // Only export trading splits when exporting a trading account
        if (!is_trading_acct &&
            (xaccAccountGetType(split_acc) == ACCT_TYPE_TRADING))
        {
            continue;
        }

        CsvTransactionExportLine line(split, trans, info->separator_str, info->use_quotes, info->simple_layout, info->gdpdu_layout, is_trading_acct, ss);
        info->failed = !line.print_csv();
    }
}

static void
account_splits(CsvExportInfo *info, Account *acc,
               std::ofstream &ss, TransSet &trans_set)
{
    g_return_if_fail(info && GNC_IS_ACCOUNT(acc));
    // Setup the query for normal transaction export
    auto p1 = g_slist_prepend(g_slist_prepend(nullptr, (gpointer)TRANS_DATE_POSTED), (gpointer)SPLIT_TRANS);
    auto p2 = g_slist_prepend(nullptr, (gpointer)QUERY_DEFAULT_SORT);
    info->query = qof_query_create_for(GNC_ID_SPLIT);
    qof_query_set_book(info->query, gnc_get_current_book());
    qof_query_set_sort_order(info->query, p1, p2, nullptr);
    xaccQueryAddSingleAccountMatch(info->query, acc, QOF_QUERY_AND);
    xaccQueryAddDateMatchTT(info->query, true, info->csvd.start_time, true, info->csvd.end_time, QOF_QUERY_AND);
    export_query_splits(info, xaccAccountGetType(acc) == ACCT_TYPE_TRADING, ss, trans_set);
    qof_query_destroy(info->query);
}

/*******************************************************
 * csv_transactions_export
 *
 * write a list of transactions to a text file
 *******************************************************/
void csv_transactions_export(CsvExportInfo *info)
{
    ENTER("");
    DEBUG("File name is : %s", info->file_name);

    StringVec headers;
    bool num_action = qof_book_use_split_action_for_num_field(gnc_get_current_book());

    /* Header string */
    if (info->simple_layout)
    {
        /* Translators: The following symbols will build the header
           line of exported CSV files: */
        headers = {
            _("Date"),
            _("Account Name"),
            (num_action ? _("Transaction Number") : _("Number")),
            _("Description"),
            _("Full Category Path"),
            _("Reconcile"),
            _("Amount With Sym"),
            _("Amount Num."),
            _("Value With Sym"),
            _("Value Num."),
            _("Rate/Price"),
        };
    }
    /* Header string */
    if (info->gdpdu_layout)
    {
        /* Translators: The following symbols will build the header
           line of exported CSV files for german GdPDU-Export: */
        headers = {
            _("Date"),
            (num_action ? _("Transaction Number") : _("Number")),
            _("Debit Account"),
            _("Credit Account"),
            _("Description"),
            _("Amount"),
        };
    }
    else
        headers = {
            _("Date"),
            _("Transaction ID"),
            (num_action ? _("Transaction Number") : _("Number")),
            _("Description"),
            _("Notes"),
            _("Commodity/Currency"),
            _("Void Reason"),
            (num_action ? _("Number/Action") : _("Action")),
            _("Memo"),
            _("Full Account Name"),
            _("Account Name"),
            _("Amount With Sym"),
            _("Amount Num."),
            _("Value With Sym"),
            _("Value Num."),
            _("Reconcile"),
            _("Reconcile Date"),
            _("Rate/Price"),
        };

    /* Write header line */
    auto ss{gnc_open_filestream(info->file_name)};
    info->failed = !gnc_csv_add_line(ss, headers, info->use_quotes, info->separator_str);

    /* Go through list of accounts */
    TransSet trans_set;

    switch (info->export_type)
    {
    case XML_EXPORT_TRANS:
        for (auto ptr = info->csva.account_list; !ss.fail() && ptr; ptr = g_list_next(ptr))
            account_splits(info, GNC_ACCOUNT(ptr->data), ss, trans_set);
        break;
    case XML_EXPORT_REGISTER:
        export_query_splits(info, false, ss, trans_set);
        break;
    default:
        PERR("unknown export_type %d", info->export_type);
    }

    info->failed = ss.fail();
    LEAVE("");
}
