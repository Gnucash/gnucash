/*******************************************************************\
 * csv-transactions-export-line.h -- Convert Transaction to line    *
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

#ifndef CSV_TRANSACTIONS_EXPORT_LINE
#define CSV_TRANSACTIONS_EXPORT_LINE

#include <string>
#include <ostream>

#include "Transaction.h"
#include "csv-export-helpers.hpp"

class CsvTransactionExportLine
{
public:
    CsvTransactionExportLine(Split *split,
                             Transaction *transaction,
                             const char *separator,
                             bool use_quotes,
                             bool simple,
                             bool gdpdu,
                             bool is_trading_acc,
                             std::ofstream &ss);

    bool print_csv();

protected:
    std::string get_date(Transaction *trans);
    std::string get_guid(Transaction *trans);
    std::string get_reconcile_date(Split *split);
    std::string get_account_name(Split *split, bool full);
    std::string get_account_number(Split *split);
    std::string get_other_account_number(Split *split);
    std::string get_number(Transaction *trans);
    std::string get_description(Transaction *trans);
    std::string get_notes(Transaction *trans);
    std::string get_void_reason(Transaction *trans);
    std::string get_memo(Split *split);
    std::string get_category(Split *split, bool full);
    std::string get_action(Split *split);
    std::string get_reconcile(Split *split);
    std::string get_commodity(Transaction *trans);
    std::string get_amount(Split *split, bool t_void, bool symbol);
    std::string get_value(Split *split, bool t_void, bool symbol);
    std::string get_rate(Split *split, bool t_void);
    std::string get_price(Split *split, bool t_void);

    bool is_split_transaction();

    StringVec make_simple_trans_split_line(Split *split);
    StringVec make_simple_trans_line(Split *split);
    StringVec make_gdpdu_trans_split_line(Split *split);
    StringVec make_gdpdu_trans_line(Split *split);   
    StringVec make_complex_trans_line(Split *split);

private:
    Split *m_split;
    Transaction *m_transaction;

    const char *m_separator;
    bool m_use_quotes;
    bool m_simple;
    bool m_gdpdu;
    bool m_is_trading_acc;
    std::ofstream &m_ss;

    bool m_is_debit_split;
    bool m_is_credit_split;

    Split *m_base_split;
    Account *m_base_split_account;
};

#endif