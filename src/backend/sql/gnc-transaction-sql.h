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

#ifndef GNC_TRANSACTION_SQL_H_
#define GNC_TRANSACTION_SQL_H_

#include "qof.h"
#include <gmodule.h>

void gnc_sql_init_transaction_handler( void );

/**
 * Commits all of the splits for a transaction.
 *
 * @param be SQL backend
 * @param pTx Transaction
 */
void gnc_sql_transaction_commit_splits( GncSqlBackend* be, Transaction* pTx );

/**
 * Saves a transaction to the db.
 *
 * @param be SQL backend
 * @param inst Transaction instance
 * @return TRUE if successful, FALSE if unsuccessful
 */
gboolean gnc_sql_save_transaction( GncSqlBackend* be, QofInstance* inst );

/**
 * Loads all transactions which have splits for a specific account.
 *
 * @param be SQL backend
 * @param account Account
 */
void gnc_sql_transaction_load_tx_for_account( GncSqlBackend* be, Account* account );

/**
 * Loads all transactions.
 *
 * @param be SQL backend
 */
void gnc_sql_transaction_load_all_tx( GncSqlBackend* be );

typedef struct {
	Account* acct;
	gnc_numeric balance;
	gnc_numeric cleared_balance;
	gnc_numeric reconciled_balance;
} acct_balances_t;

/**
 * Returns a list of acct_balances_t structures, one for each account which
 * has splits.
 *
 * @param be SQL backend
 * @return GSList of acct_balances_t structures
 */
/*@ null @*/ GSList* gnc_sql_get_account_balances_slist( GncSqlBackend* be );

#endif /* GNC_TRANSACTION_SQL_H_ */
