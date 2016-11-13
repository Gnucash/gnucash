/********************************************************************
 * gnc-transaction-sql.h: load and save data to SQL                 *
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
/** @file gnc-transaction-sql.h
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#ifndef GNC_TRANSACTION_SQL_H
#define GNC_TRANSACTION_SQL_H

extern "C"
{
#include "Transaction.h"
#include "qof.h"
#include "Account.h"
}
class GncSqlTransBackend : public GncSqlObjectBackend
{
public:
    GncSqlTransBackend();
    void load_all(GncSqlBackend*) override;
    void create_tables(GncSqlBackend*) override;
    bool commit (GncSqlBackend* sql_be, QofInstance* inst) override;
};

class GncSqlSplitBackend : public GncSqlObjectBackend
{
public:
    GncSqlSplitBackend();
    void load_all(GncSqlBackend*) override { return; } // loaded by transaction.
    void create_tables(GncSqlBackend*) override;
    bool commit (GncSqlBackend* sql_be, QofInstance* inst) override;
};

/**
 * Loads all transactions which have splits for a specific account.
 *
 * @param sql_be SQL backend
 * @param account Account
 */
void gnc_sql_transaction_load_tx_for_account (GncSqlBackend* sql_be,
                                              Account* account);
typedef struct
{
    Account* acct;
    gnc_numeric balance;
    gnc_numeric cleared_balance;
    gnc_numeric reconciled_balance;
} acct_balances_t;

/**
 * Returns a list of acct_balances_t structures, one for each account which
 * has splits.
 *
 * @param sql_be SQL backend
 * @return GSList of acct_balances_t structures
 */
GSList* gnc_sql_get_account_balances_slist (GncSqlBackend* sql_be);

#endif /* GNC_TRANSACTION_SQL_H */
