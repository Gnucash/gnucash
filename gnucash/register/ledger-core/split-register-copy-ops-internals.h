/********************************************************************\
 * split-register-copy-ops-internals.c -- internal details of       *
 *                 copy/paste semantics for transactions and splits *
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

#ifndef SPLIT_REGISTER_COPY_OPS_INTERNAL_H
#define SPLIT_REGISTER_COPY_OPS_INTERNAL_H

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
} floating_split;

typedef struct
{
    Transaction *m_txn;
    gnc_commodity *m_currency;
    time64 m_date_entered;
    time64 m_date_posted;
    const char *m_num;
    const char *m_description;
    const char *m_notes;
    const char *m_association;
    SplitList *m_splits;
} floating_txn;

/* constructor */
floating_split *gnc_make_float_split (void);

/* type predicate */
gboolean is_gnc_float_split (const floating_split *fs);

/* accessors */
Split *gnc_float_split_get_split(const floating_split* fs);
Account *gnc_float_split_get_account (const floating_split *fs); /* direct account pointer rather than account guid */
Transaction *gnc_float_split_get_transaction (const floating_split *fs); /* direct transaction pointer rather than transaction guid */
const char *gnc_float_split_get_memo (const floating_split *fs);
const char *gnc_float_split_get_action (const floating_split *fs);
char gnc_float_split_get_reconcile_state (const floating_split *fs);
time64 gnc_float_split_get_reconcile_date (const floating_split *fs);
gnc_numeric gnc_float_split_get_amount (const floating_split *fs);
gnc_numeric gnc_float_split_get_value (const floating_split *fs);

/* modifiers */
void gnc_float_split_set_split (floating_split *fs, Split *split);
void gnc_float_split_set_account (floating_split *fs, Account *account); /* direct account pointer rather than account guid */
void gnc_float_split_set_transaction (floating_split *fs, Transaction *transaction); /* direct transaction pointer rather than transaction guid */
void gnc_float_split_set_memo (floating_split *fs, const char *memo);
void gnc_float_split_set_action (floating_split *fs, const char *action);
void gnc_float_split_set_reconcile_state (floating_split *fs, char reconcile_state);
void gnc_float_split_set_reconcile_date (floating_split *fs, time64 reconcile_date);
void gnc_float_split_set_amount (floating_split *fs, gnc_numeric amount);
void gnc_float_split_set_value (floating_split *fs, gnc_numeric value);

/* Scheme: gnc:split->split-scm */
floating_split *gnc_split_to_float_split (Split *split);
/* Scheme: gnc:split-scm-onto-split */
void gnc_float_split_to_split (const floating_split *fs, Split *split);


struct floating_txn;

/* constructor */
floating_txn *gnc_make_float_txn (void);

/* type predicate */
gboolean is_gnc_float_txn (const floating_txn *ft);

/* accessors */
Transaction *gnc_float_txn_get_txn (const floating_txn *ft);
gnc_commodity *gnc_float_txn_get_currency (const floating_txn *ft);
time64 gnc_float_txn_get_date_entered (const floating_txn *ft);
time64 gnc_float_txn_get_date_posted (const floating_txn *ft);
const char *gnc_float_txn_get_num (const floating_txn *ft);
const char *gnc_float_txn_get_description (const floating_txn *ft);
const char *gnc_float_txn_get_notes (const floating_txn *ft);
const char *gnc_float_txn_get_association (const floating_txn *ft);
SplitList *gnc_float_txn_get_splits (const floating_txn *ft);

floating_split *gnc_float_txn_get_float_split (const floating_txn *ft, guint index);
floating_split *gnc_float_txn_get_other_float_split (const floating_txn *ft, floating_split *fs);

/* modifiers */
void gnc_float_txn_set_txn (floating_txn *ft, Transaction *txn);
void gnc_float_txn_set_currency (floating_txn *ft, gnc_commodity *currency);
void gnc_float_txn_set_date_entered (floating_txn *ft, time64 date_entered);
void gnc_float_txn_set_date_posted (floating_txn *ft, time64 date_posted);
void gnc_float_txn_set_num (floating_txn *ft, const char *num);
void gnc_float_txn_set_description (floating_txn *ft, const char *description);
void gnc_float_txn_set_notes (floating_txn *ft, const char *notes);
void gnc_float_txn_set_association (floating_txn *ft, const char *association);
void gnc_float_txn_set_splits (floating_txn *ft, SplitList *splits);

void gnc_float_txn_append_float_split (floating_txn *ft, floating_split *fs);

/* Scheme: gnc:transaction->transaction-scm */
floating_txn *gnc_txn_to_float_txn (Transaction *txn, gboolean use_cut_semantics);

/* Scheme: gnc:transaction-scm-onto-transaction */
void gnc_float_txn_to_txn (const floating_txn *ft, Transaction *txn, GHashTable* account_map, gboolean commit, QofBook *book);

#endif
