/********************************************************************
 * gnc-transaction-gda.h: load and save data to SQL via libgda      *
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
/** @file gnc-transaction-gda.h
 *  @brief load and save data to SQL via libgda
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database via libgda
 */

#ifndef GNC_TRANSACTION_GDA_H_
#define GNC_TRANSACTION_GDA_H_

#include "qof.h"
#include <gmodule.h>

void gnc_gda_init_transaction_handler( void );
void gnc_gda_transaction_commit_splits( GncGdaBackend* be, Transaction* pTx );
void gnc_gda_save_transaction( QofInstance* inst, GncGdaBackend* be );
void gnc_gda_get_account_balances( GncGdaBackend* be, Account* pAccount, 
								    gnc_numeric* start_balance,
								    gnc_numeric* cleared_balance,
									gnc_numeric* reconciled_balance );

#endif /* GNC_TRANSACTION_GDA_H_ */
