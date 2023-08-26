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
#include "csv-export-helpers.hpp"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_ASSISTANT;


/*******************************************************************/

/******************** Helper functions *********************/

static std::string
get_date (Transaction *trans)
{
    char datebuff [MAX_DATE_LENGTH + 1];
    qof_print_date_buff(datebuff, MAX_DATE_LENGTH, xaccTransGetDate (trans));
    return datebuff;
}


static std::string
get_guid (Transaction *trans)
{
    return gnc::GUID (*qof_entity_get_guid (QOF_INSTANCE (trans))).to_string();
}

// Reconcile Date
static std::string
get_reconcile_date (Split *split)
{
    if (xaccSplitGetReconcile (split) != YREC)
        return "";

    char datebuff[MAX_DATE_LENGTH + 1];
    qof_print_date_buff (datebuff, MAX_DATE_LENGTH, xaccSplitGetDateReconciled (split));
    return datebuff;
}

// Account Name short or Long
static std::string
get_account_name (Split *split, bool full)
{
    auto account{xaccSplitGetAccount (split)};
    return full ? account_get_fullname_str (account) : xaccAccountGetName (account);
}

// Number
static std::string
get_number (Transaction *trans)
{
    auto num{xaccTransGetNum (trans)};
    return (num ? num : "");
}

// Description
static std::string
get_description (Transaction *trans)
{
    auto desc{xaccTransGetDescription (trans)};
    return (desc ? desc : "");
}

// Notes
static std::string
get_notes (Transaction *trans)
{
    auto notes{xaccTransGetNotes (trans)};
    return (notes ? notes : "");
}

// Void reason
static std::string
get_void_reason (Transaction *trans)
{
    auto void_reason{xaccTransGetVoidReason (trans)};
    return (void_reason ? void_reason : "");
}

// Memo
static std::string
get_memo (Split *split)
{
    auto memo{xaccSplitGetMemo (split)};
    return (memo ? memo : "");
}

// Full Category Path or Not
static std::string
get_category (Split *split, bool full)
{
    auto other{xaccSplitGetOtherSplit(split)};
    return other ? get_account_name (other, full) : _("-- Split Transaction --");
}

// Action
static std::string
get_action (Split *split)
{
    auto action{xaccSplitGetAction (split)};
    return (action ? action : "");
}

// Reconcile
static std::string
get_reconcile (Split *split)
{
    auto recon{gnc_get_reconcile_str (xaccSplitGetReconcile (split))};
    return (recon ? recon : "");
}

// Transaction commodity
static std::string
get_commodity (Transaction *trans)
{
    return gnc_commodity_get_unique_name (xaccTransGetCurrency (trans));
}

// Amount with Symbol or not
static std::string
get_amount (Split *split, bool t_void, bool symbol)
{
    auto amt_num{t_void ? xaccSplitVoidFormerAmount (split) : xaccSplitGetAmount (split)};
    return xaccPrintAmount (amt_num, gnc_split_amount_print_info (split, symbol));
}

// Value with Symbol or not
static std::string
get_value (Split *split, bool t_void, bool symbol)
{
    auto trans{xaccSplitGetParent(split)};
    auto tcurr{xaccTransGetCurrency (trans)};
    auto pai{gnc_commodity_print_info (tcurr, symbol)};
    auto amt_num{t_void ? xaccSplitVoidFormerValue (split): xaccSplitGetValue (split)};
    return xaccPrintAmount (amt_num, pai);
}

// Share Price / Conversion factor
static std::string
get_rate (Split *split, bool t_void)
{
    auto curr{xaccAccountGetCommodity (xaccSplitGetAccount (split))};
    auto amt_num{t_void ? gnc_numeric_zero() : xaccSplitGetSharePrice (split)};
    return xaccPrintAmount (amt_num, gnc_default_price_print_info (curr));
}

// Share Price / Conversion factor
static std::string
get_price (Split *split, bool t_void)
{
    auto curr{xaccAccountGetCommodity (xaccSplitGetAccount (split))};
    auto cf{t_void
            ? gnc_numeric_div (xaccSplitVoidFormerValue (split),
                               xaccSplitVoidFormerAmount (split),
                               GNC_DENOM_AUTO,
                               GNC_HOW_DENOM_SIGFIGS(6) | GNC_HOW_RND_ROUND_HALF_UP)
            : xaccSplitGetSharePrice (split)};
    return xaccPrintAmount (cf, gnc_default_price_print_info (curr));
}

/******************************************************************************/

static StringVec
make_simple_trans_line (Transaction *trans, Split *split)
{
    auto t_void{xaccTransGetVoidStatus (trans)};
    return {
        get_date (trans),
        get_account_name (split, true),
        get_number (trans),
        get_description (trans),
        get_category (split, true),
        get_reconcile (split),
        get_amount (split, t_void, true),
        get_amount (split, t_void, false),
        get_value (split, t_void, true),
        get_value (split, t_void, false),
        get_rate (split, t_void)
    };
}

static StringVec
make_complex_trans_line (Transaction *trans, Split *split)
{
    auto t_void{xaccTransGetVoidStatus (trans)};
    return {
        get_date (trans),
        get_guid (trans),
        get_number (trans),
        get_description (trans),
        get_notes (trans),
        get_commodity (trans),
        get_void_reason (trans),
        get_action (split),
        get_memo (split),
        get_account_name (split, true),
        get_account_name (split, false),
        get_amount (split, t_void, true),
        get_amount (split, t_void, false),
        get_value (split, t_void, true),
        get_value (split, t_void, false),
        get_reconcile (split),
        get_reconcile_date (split),
        get_price (split, t_void)
    };
}

using TransSet = std::unordered_set<Transaction*>;

/*******************************************************
 * account_splits
 *
 * gather the splits / transactions for an account and
 * send them to a file
 *******************************************************/
static void
export_query_splits (CsvExportInfo *info, bool is_trading_acct,
                     std::ofstream& ss, TransSet& trans_set)
{
    g_return_if_fail (info);

    /* Run the query */
    for (GList *splits = qof_query_run (info->query); !info->failed && splits;
         splits = splits->next)
    {
        auto split{static_cast<Split*>(splits->data)};
        auto trans{xaccSplitGetParent (split)};

        // Look for trans already exported in trans_set
        if (!trans_set.emplace (trans).second)
            continue;

        // Look for blank split
        Account *split_acc = xaccSplitGetAccount (split);
        if (!split_acc)
            continue;

        // Only export trading splits when exporting a trading account
        if (!is_trading_acct &&
            (xaccAccountGetType (split_acc) == ACCT_TYPE_TRADING))
            continue;

        if (info->simple_layout)
        {
            // Write line in simple layout, equivalent to a single line register view
            auto line = make_simple_trans_line (trans, split);
            info->failed = !gnc_csv_add_line (ss, line, info->use_quotes,
                                              info->separator_str);
            continue;
        }

        // Write complex Transaction Line.
        auto line = make_complex_trans_line (trans, split);
        info->failed = !gnc_csv_add_line (ss, line, info->use_quotes,
                                          info->separator_str);

        /* Loop through the list of splits for the Transaction */
        for (auto node = xaccTransGetSplitList (trans); !info->failed && node;
             node = node->next)
        {
            auto t_split{static_cast<Split*>(node->data)};

            // base split is already written on the trans_line
            if (split == t_split)
                continue;

            // Only export trading splits if exporting a trading account
            Account *tsplit_acc = xaccSplitGetAccount (t_split);
            if (!is_trading_acct &&
                (xaccAccountGetType (tsplit_acc) == ACCT_TYPE_TRADING))
                continue;

            // Write complex Split Line.
            auto line = make_complex_trans_line (trans, t_split);
            info->failed = !gnc_csv_add_line (ss, line, info->use_quotes,
                                              info->separator_str);
        }
    }
}

static void
account_splits (CsvExportInfo *info, Account *acc,
                std::ofstream& ss, TransSet& trans_set)
{
    g_return_if_fail (info && GNC_IS_ACCOUNT (acc));
    // Setup the query for normal transaction export
    auto p1 = g_slist_prepend (g_slist_prepend (nullptr, (gpointer)TRANS_DATE_POSTED), (gpointer)SPLIT_TRANS);
    auto p2 = g_slist_prepend (nullptr, (gpointer)QUERY_DEFAULT_SORT);
    info->query = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (info->query, gnc_get_current_book());
    qof_query_set_sort_order (info->query, p1, p2, nullptr);
    xaccQueryAddSingleAccountMatch (info->query, acc, QOF_QUERY_AND);
    xaccQueryAddDateMatchTT (info->query, true, info->csvd.start_time, true, info->csvd.end_time, QOF_QUERY_AND);
    export_query_splits (info, xaccAccountGetType (acc) == ACCT_TYPE_TRADING, ss, trans_set);
    qof_query_destroy (info->query);
}

/*******************************************************
 * csv_transactions_export
 *
 * write a list of transactions to a text file
 *******************************************************/
void csv_transactions_export (CsvExportInfo *info)
{
    ENTER("");
    DEBUG("File name is : %s", info->file_name);

    StringVec headers;
    bool num_action = qof_book_use_split_action_for_num_field (gnc_get_current_book());

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
    info->failed = !gnc_csv_add_line (ss, headers, info->use_quotes, info->separator_str);

    /* Go through list of accounts */
    TransSet trans_set;

    switch (info->export_type)
    {
    case XML_EXPORT_TRANS:
        for (auto ptr = info->csva.account_list; !ss.fail() && ptr; ptr = g_list_next(ptr))
            account_splits (info, GNC_ACCOUNT(ptr->data), ss, trans_set);
        break;
    case XML_EXPORT_REGISTER:
        export_query_splits (info, false, ss, trans_set);
        break;
    default:
        PERR ("unknown export_type %d", info->export_type);
    }

    info->failed = ss.fail();
    LEAVE("");
}

