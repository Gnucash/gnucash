/*******************************************************************\
 * csv-transactions-export-line.cpp -- Convert Transaction to line    *
 *                                                                  *
 * Copyright (C) 2025 Johannes Triegel                                *
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
/** @file csv-transactions-export.h
    @brief CSV Export Transactions
    @author Copyright (c) 2025 Johannes Triegel
*/

#include <iostream>

#include <gnc-filepath-utils.h>
#include "gnc-commodity.h"
#include "gnc-ui-util.h"
#include "Query.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "qofbookslots.h"
#include "guid.hpp"

#include "csv-transaction-export-line.hpp"

CsvTransactionExportLine::CsvTransactionExportLine(Split *split,
                                                   Transaction *transaction,
                                                   const char *separator,
                                                   bool use_quotes,
                                                   bool simple,
                                                   std::ofstream &ss) : m_split(split),
                                                                        m_transaction(transaction),
                                                                        m_separator(separator),
                                                                        m_use_quotes(use_quotes),
                                                                        m_simple(simple),
                                                                        m_ss(ss),
                                                                        m_is_debit_split(false),
                                                                        m_is_credit_split(false),
                                                                        m_base_split(NULL),
                                                                        m_base_split_account(NULL)
{
    GList *splits = xaccTransGetSplitList(m_transaction);
    uint debit = 0;
    uint credit = 0;

    if (g_list_length(splits) > 2)
    {
        /* Check if the base is a debit or credit booking, e.g. which is the account everything comes to or from */
        for (GList *split = splits; split != NULL; split = split->next)
        {
            gnc_numeric amount = xaccSplitGetAmount(static_cast<Split *>(split->data));
            if (gnc_numeric_positive_p(amount))
            {
                debit++;
            }
            else if (gnc_numeric_negative_p(amount))
            {
                credit++;
            }
        }
        if (debit > 1 && credit > 1)
        {
            std::cerr << "Multi split booking that needs to be booked on an intermediate account, no implemented!" << std::endl;
            return;
        }
        else if (credit > 1)
        {
            m_is_debit_split = true;
        }
        else
        {
            m_is_credit_split = true;
        }

        // Find the base_split
        Split *m_base_split = NULL;
        for (GList *split = splits; split != NULL; split = split->next)
        {
            gnc_numeric amount = xaccSplitGetAmount(static_cast<Split *>(split->data));
            if (gnc_numeric_positive_p(amount) && m_is_debit_split)
            {
                m_base_split = static_cast<Split *>(split->data);
                break;
            }
            else if (gnc_numeric_negative_p(amount) && m_is_credit_split)
            {
                m_base_split = static_cast<Split *>(split->data);
                break;
            }
        }
        m_base_split_account = xaccSplitGetAccount(m_base_split);
    }
    else
    {
        m_is_debit_split = false;
        m_is_credit_split = false;
        m_base_split_account = NULL;
        m_base_split = NULL;
    }
}

bool CsvTransactionExportLine::is_split_transaction()
{
    return m_is_debit_split || m_is_credit_split;
}

std::string
CsvTransactionExportLine::get_date(Transaction *trans)
{
    char datebuff[MAX_DATE_LENGTH + 1];
    qof_print_date_buff(datebuff, MAX_DATE_LENGTH, xaccTransGetDate(m_transaction));
    return datebuff;
}

std::string
CsvTransactionExportLine::get_guid(Transaction *trans)
{
    return gnc::GUID(*qof_entity_get_guid(QOF_INSTANCE(m_transaction))).to_string();
}

// Reconcile Date
std::string
CsvTransactionExportLine::get_reconcile_date(Split *split)
{
    if (xaccSplitGetReconcile(split) != YREC)
        return "";

    char datebuff[MAX_DATE_LENGTH + 1];
    qof_print_date_buff(datebuff, MAX_DATE_LENGTH, xaccSplitGetDateReconciled(split));
    return datebuff;
}

// Account Name short or Long
std::string
CsvTransactionExportLine::get_account_name(Split *split, bool full)
{
    auto account{xaccSplitGetAccount(split)};
    return full ? account_get_fullname_str(account) : xaccAccountGetName(account);
}

// Number
std::string
CsvTransactionExportLine::get_number(Transaction *trans)
{
    auto num{xaccTransGetNum(m_transaction)};
    return (num ? num : "");
}

// Description
std::string
CsvTransactionExportLine::get_description(Transaction *trans)
{
    auto desc{xaccTransGetDescription(m_transaction)};
    return (desc ? desc : "");
}

// Notes
std::string
CsvTransactionExportLine::get_notes(Transaction *trans)
{
    auto notes{xaccTransGetNotes(m_transaction)};
    return (notes ? notes : "");
}

// Void reason
std::string
CsvTransactionExportLine::get_void_reason(Transaction *trans)
{
    auto void_reason{xaccTransGetVoidReason(m_transaction)};
    return (void_reason ? void_reason : "");
}

// Memo
std::string
CsvTransactionExportLine::get_memo(Split *split)
{
    auto memo{xaccSplitGetMemo(split)};
    return (memo ? memo : "");
}

// Full Category Path or Not
std::string
CsvTransactionExportLine::get_category(Split *split, bool full)
{
    auto other{xaccSplitGetOtherSplit(split)};
    return other ? get_account_name(other, full) : _("-- Split Transaction --");
}

// Action
std::string
CsvTransactionExportLine::get_action(Split *split)
{
    auto action{xaccSplitGetAction(split)};
    return (action ? action : "");
}

// Reconcile
std::string
CsvTransactionExportLine::get_reconcile(Split *split)
{
    auto recon{gnc_get_reconcile_str(xaccSplitGetReconcile(split))};
    return (recon ? recon : "");
}

// Transaction commodity
std::string
CsvTransactionExportLine::get_commodity(Transaction *trans)
{
    return gnc_commodity_get_unique_name(xaccTransGetCurrency(m_transaction));
}

// Amount with Symbol or not
std::string
CsvTransactionExportLine::get_amount(Split *split, bool t_void, bool symbol)
{
    auto amt_num{t_void ? xaccSplitVoidFormerAmount(split) : xaccSplitGetAmount(split)};
    auto pinfo{gnc_split_amount_print_info(split, symbol)};
    if (!symbol)
        pinfo.use_separators = 0;
    return xaccPrintAmount(amt_num, pinfo);
}

// Value with Symbol or not
std::string
CsvTransactionExportLine::get_value(Split *split, bool t_void, bool symbol)
{
    auto tcurr{xaccTransGetCurrency(m_transaction)};
    auto pai{gnc_commodity_print_info(tcurr, symbol)};
    if (!symbol)
        pai.use_separators = 0;
    auto amt_num{t_void ? xaccSplitVoidFormerValue(split) : xaccSplitGetValue(split)};
    return xaccPrintAmount(amt_num, pai);
}

// Share Price / Conversion factor
std::string
CsvTransactionExportLine::get_rate(Split *split, bool t_void)
{
    auto curr{xaccAccountGetCommodity(xaccSplitGetAccount(split))};
    auto amt_num{t_void ? gnc_numeric_zero() : xaccSplitGetSharePrice(split)};
    return xaccPrintAmount(amt_num, gnc_default_price_print_info(curr));
}

// Share Price / Conversion factor
std::string
CsvTransactionExportLine::get_price(Split *split, bool t_void)
{
    auto curr{xaccAccountGetCommodity(xaccSplitGetAccount(split))};
    auto cf{t_void
                ? gnc_numeric_div(xaccSplitVoidFormerValue(split),
                                  xaccSplitVoidFormerAmount(split),
                                  GNC_DENOM_AUTO,
                                  GNC_HOW_DENOM_SIGFIGS(6) | GNC_HOW_RND_ROUND_HALF_UP)
                : xaccSplitGetSharePrice(split)};
    return xaccPrintAmount(cf, gnc_default_price_print_info(curr));
}

bool CsvTransactionExportLine::print_csv()
{
    if (!is_split_transaction() && m_simple)
    {
        auto line = make_simple_trans_line(m_split);
        return gnc_csv_add_line(m_ss, line, m_use_quotes,
                                m_separator);
    }
    else
    {
        if (m_simple)
        {
            bool ok = false;
            GList *splits = xaccTransGetSplitList(m_transaction);
            for (GList *split = splits; split != NULL; split = split->next)
            {
                Split *s = static_cast<Split *>(split->data);

                auto split_account = xaccSplitGetAccount(s);
                if (!split_account)
                {
                    continue;
                }

                if (xaccAccountEqual(split_account, m_base_split_account, true))
                    continue;

                auto line = make_simple_trans_split_line(s);
                ok = gnc_csv_add_line(m_ss, line, m_use_quotes,
                                      m_separator);
                if (!ok)
                    break;
            }
            return ok;
        }
        else
        {
            return false;
        }
    }
}

StringVec
CsvTransactionExportLine::make_simple_trans_split_line(Split *split)
{
    auto t_void{xaccTransGetVoidStatus(m_transaction)};
    gnc_numeric amount = xaccSplitGetAmount(split);
    bool pos = gnc_numeric_positive_p(amount);
    return {
        get_date(m_transaction),
        pos ? xaccAccountGetName(m_base_split_account) : get_account_name(split, true),
        get_number(m_transaction),
        get_description(m_transaction),
        pos ? get_account_name(split, true) : xaccAccountGetName(m_base_split_account),
        get_reconcile(split),
        get_amount(split, t_void, true),
        get_amount(split, t_void, false),
        get_value(split, t_void, true),
        get_value(split, t_void, false),
        get_rate(split, t_void)};
}

StringVec
CsvTransactionExportLine::make_complex_trans_line(Split *split)
{
    auto t_void{xaccTransGetVoidStatus(m_transaction)};
    return {
        get_date(m_transaction),
        get_guid(m_transaction),
        get_number(m_transaction),
        get_description(m_transaction),
        get_notes(m_transaction),
        get_commodity(m_transaction),
        get_void_reason(m_transaction),
        get_action(split),
        get_memo(split),
        get_account_name(split, true),
        get_account_name(split, false),
        get_amount(split, t_void, true),
        get_amount(split, t_void, false),
        get_value(split, t_void, true),
        get_value(split, t_void, false),
        get_reconcile(split),
        get_reconcile_date(split),
        get_price(split, t_void)};
}

StringVec
CsvTransactionExportLine::make_simple_trans_line(Split *split)
{
    auto t_void{xaccTransGetVoidStatus(m_transaction)};
    return {
        get_date(m_transaction),
        get_account_name(split, true),
        get_number(m_transaction),
        get_description(m_transaction),
        get_category(split, true),
        get_reconcile(split),
        get_amount(split, t_void, true),
        get_amount(split, t_void, false),
        get_value(split, t_void, true),
        get_value(split, t_void, false),
        get_rate(split, t_void)};
}