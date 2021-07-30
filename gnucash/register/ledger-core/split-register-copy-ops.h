/********************************************************************\
 * split-register-copy-ops.c -- copy/paste semantics for            *
 *                                         transactions and splits  *
 * Port to C of engine-interface                                    *
 * originally written by Dave Peticolas <dave@krondo.com>           *
 * © 2019 Geert Janssens
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 * \********************************************************************/

#ifndef SPLIT_REGISTER_COPY_OPS_H
#define SPLIT_REGISTER_COPY_OPS_H

#include <glib.h>

#include "gnc-engine.h"   /* for typedefs */
#include "qof.h"

#include "guid.h"
#include "Split.h"
#include "Account.h"
#include "Transaction.h"

typedef struct
{
    Split       *m_split;
    Account     *m_account;
    Transaction *m_transaction;
    const char  *m_memo;
    const char  *m_action;
    time64       m_reconcile_date;
    char         m_reconcile_state;
    gnc_numeric  m_value;
    gnc_numeric  m_amount;
} FloatingSplit;

typedef struct
{
    Transaction *m_txn;
    gnc_commodity *m_currency;
    time64 m_date_entered;
    time64 m_date_posted;
    const char *m_num;
    const char *m_description;
    const char *m_notes;
    const char *m_doclink;
    SplitList *m_splits;
} FloatingTxn;

/* accessors */
Split *gnc_float_split_get_split(const FloatingSplit* fs);
Account *gnc_float_split_get_account (const FloatingSplit *fs); /* direct account pointer rather than account guid */
Transaction *gnc_float_split_get_transaction (const FloatingSplit *fs); /* direct transaction pointer rather than transaction guid */
const char *gnc_float_split_get_memo (const FloatingSplit *fs);
const char *gnc_float_split_get_action (const FloatingSplit *fs);
char gnc_float_split_get_reconcile_state (const FloatingSplit *fs);
time64 gnc_float_split_get_reconcile_date (const FloatingSplit *fs);
gnc_numeric gnc_float_split_get_amount (const FloatingSplit *fs);
gnc_numeric gnc_float_split_get_value (const FloatingSplit *fs);

/* modifiers */
void gnc_float_split_set_split (FloatingSplit *fs, Split *split);
void gnc_float_split_set_account (FloatingSplit *fs, Account *account); /* direct account pointer rather than account guid */
void gnc_float_split_set_transaction (FloatingSplit *fs, Transaction *transaction); /* direct transaction pointer rather than transaction guid */
void gnc_float_split_set_memo (FloatingSplit *fs, const char *memo);
void gnc_float_split_set_action (FloatingSplit *fs, const char *action);
void gnc_float_split_set_reconcile_state (FloatingSplit *fs, char reconcile_state);
void gnc_float_split_set_reconcile_date (FloatingSplit *fs, time64 reconcile_date);
void gnc_float_split_set_amount (FloatingSplit *fs, gnc_numeric amount);
void gnc_float_split_set_value (FloatingSplit *fs, gnc_numeric value);

FloatingSplit *gnc_split_to_float_split (Split *split);
void gnc_float_split_to_split (const FloatingSplit *fs, Split *split);

void gnc_float_split_free (FloatingSplit *fs);

/* accessors */
Transaction *gnc_float_txn_get_txn (const FloatingTxn *ft);
gnc_commodity *gnc_float_txn_get_currency (const FloatingTxn *ft);
time64 gnc_float_txn_get_date_entered (const FloatingTxn *ft);
time64 gnc_float_txn_get_date_posted (const FloatingTxn *ft);
const char *gnc_float_txn_get_num (const FloatingTxn *ft);
const char *gnc_float_txn_get_description (const FloatingTxn *ft);
const char *gnc_float_txn_get_notes (const FloatingTxn *ft);
const char *gnc_float_txn_get_doclink (const FloatingTxn *ft);
SplitList *gnc_float_txn_get_splits (const FloatingTxn *ft);

FloatingSplit *gnc_float_txn_get_float_split (const FloatingTxn *ft, guint index);
FloatingSplit *gnc_float_txn_get_other_float_split (const FloatingTxn *ft, FloatingSplit *fs);

/* modifiers */
void gnc_float_txn_set_txn (FloatingTxn *ft, Transaction *txn);
void gnc_float_txn_set_currency (FloatingTxn *ft, gnc_commodity *currency);
void gnc_float_txn_set_date_entered (FloatingTxn *ft, time64 date_entered);
void gnc_float_txn_set_date_posted (FloatingTxn *ft, time64 date_posted);
void gnc_float_txn_set_num (FloatingTxn *ft, const char *num);
void gnc_float_txn_set_description (FloatingTxn *ft, const char *description);
void gnc_float_txn_set_notes (FloatingTxn *ft, const char *notes);
void gnc_float_txn_set_doclink (FloatingTxn *ft, const char *doclink);
void gnc_float_txn_set_splits (FloatingTxn *ft, SplitList *splits);

void gnc_float_txn_append_float_split (FloatingTxn *ft, FloatingSplit *fs);

FloatingTxn *gnc_txn_to_float_txn (Transaction *txn, gboolean use_cut_semantics);

void gnc_float_txn_to_txn (const FloatingTxn *ft, Transaction *txn, gboolean do_commit);
void gnc_float_txn_to_txn_swap_accounts (const FloatingTxn *ft, Transaction *txn, Account *acct1, Account *acct2, gboolean do_commit);

void gnc_float_txn_free (FloatingTxn *ft);

#endif
