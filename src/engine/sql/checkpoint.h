/********************************************************************\
 * checkpoint.h -- balance checkpoints                              *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

/* 
 * FILE:
 * checkpoint.h
 *
 * FUNCTION:
 * Defines balance checkpoints
 *
 * HISTORY:
 * Copyright (c) 2001 Linas Vepstas 
 */


#ifndef __CHECKPOINT_H__
#define __CHECKPOINT_H__

#include "Account.h"
#include "Group.h"
#include "guid.h"
#include "Transaction.h"

#include "PostgresBackend.h"

/* -------------------------------------------------------- */
/* The balance checkpoint structure is used to store partial,
 * running balances.  The balances are correct for the checkpoint
 * date shown.  The commodity indicates what commodity the 
 * balances are valued in (they need not be in the same 
 * commodity as the account)
 */

/* the MIN_CHECKPOINT_COUNT value is the number of splits that
 * each checkpoint will handle, on avergage.  30 seems like a good
 * number.  The number of splits in a checkpoint will vary; 
 * checkpoints can onmly occur in between entry dates, so a 
 * bunch of entries with the same date will go into the same 
 * checkpoint (and there might be an arbitrarily large number of these)
 */
#define MIN_CHECKPOINT_COUNT 3

typedef struct _checkpoint {
   const GUID *account_guid;
   const char * commodity;
   Timespec date_start;
   Timespec date_end;
   gint64 balance;
   gint64 cleared_balance;
   gint64 reconciled_balance;
} Checkpoint;

/* -------------------------------------------------------- */
/* function prototypes */

#define CK_EARLIEST_DATE "1903-01-02 08:35:46.00"
#define CK_AFTER_EARLIEST_DATE "1903-01-03 03:03:03.00"
#define CK_LAST_DATE "2038-01-02 08:35:46.00"
#define CK_AFTER_LAST_DATE "2038-01-02 12:12:12.00"


void pgendTransactionRecomputeCheckpoints (PGBackend *be, Transaction *trans);
void pgendAccountRecomputeOneCheckpoint (PGBackend *be, Account *acc, Timespec ts);
void pgendGroupRecomputeAllCheckpoints (PGBackend *, AccountGroup *);
void pgendGroupGetAllBalances (PGBackend *, AccountGroup *, Timespec as_of_date);

/* The pgendAccountGetBalance() routine goes to the sql database and finds the
 *    checkpoint with the latest date before the 'as_of_date' argument. 
 *    It sets the starting balance for this account based on that checkpoint.
 *    It returns the date of the starting balance.
 */
Timespec pgendAccountGetBalance (PGBackend *, Account *, Timespec as_of_date);

#endif /* __CHECKPOINT_H__ */
