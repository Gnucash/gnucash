/********************************************************************\
 * txn.h -- transaction handling routines for the postgres backend  *
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

/*
 * FILE:
 * txn.h
 *
 * FUNCTION:
 * Implements the transaction handling callbacks for the postgres backend.
 *
 * HISTORY:
 * Copyright (c) 2000, 2001 Linas Vepstas <linas@linas.org>
 */


#ifndef POSTGRES_TXN_H
#define POSTGRES_TXN_H

#include <glib.h>

#include "qof.h"
#include "Transaction.h"

#include "PostgresBackend.h"

int pgendCopyTransactionToEngine (PGBackend *be, const GUID *trans_guid);
void pgendCopySplitsToEngine (PGBackend *be, Transaction *trans);

void pgendStoreAllTransactions (PGBackend *be, Account *root);
void pgendStoreTransactionNoLock (PGBackend *be, Transaction *trans, gboolean do_check_version);

void pgend_trans_commit_edit (QofBackend * bend, Transaction * trans, Transaction * oldtrans);
void pgend_trans_rollback_edit (QofBackend * bend, Transaction * trans);




#endif /* POSTGRES_TXN_H */
