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
\********************************************************************/

#include <glib.h>

#include "config.h"
#include "split-register-copy-ops.h"

/* accessors */
Split *gnc_float_split_get_split (const FloatingSplit *fs)
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_split;
}

Account *gnc_float_split_get_account (const FloatingSplit *fs) /* direct account pointer rather than account guid */
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_account;
}

Transaction *gnc_float_split_get_transaction (const FloatingSplit *fs) /* direct transaction pointer rather than transaction guid */
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_transaction;
}

const char *gnc_float_split_get_memo (const FloatingSplit *fs)
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_memo;
}

const char *gnc_float_split_get_action (const FloatingSplit *fs)
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_action;
}

char gnc_float_split_get_reconcile_state (const FloatingSplit *fs)
{
    g_return_val_if_fail (fs, '\0');
    return fs->m_reconcile_state;
}

time64 gnc_float_split_get_reconcile_date (const FloatingSplit *fs)
{
    g_return_val_if_fail (fs, G_MAXINT64);
    return fs->m_reconcile_date;
}

gnc_numeric gnc_float_split_get_amount (const FloatingSplit *fs)
{
    g_return_val_if_fail (fs, gnc_numeric_zero());
    return fs->m_amount;
}

gnc_numeric gnc_float_split_get_value (const FloatingSplit *fs)
{
    g_return_val_if_fail (fs, gnc_numeric_zero());
    return fs->m_value;
}


/* modifiers */
void gnc_float_split_set_split(FloatingSplit *fs, Split *split)
{
    g_return_if_fail (fs);
    fs->m_split = split;
}

void gnc_float_split_set_account (FloatingSplit *fs, Account *account) /* direct account pointer rather than account guid */
{
    g_return_if_fail (fs);
    fs->m_account = account;
}

void gnc_float_split_set_transaction (FloatingSplit *fs, Transaction *transaction) /* direct transaction pointer rather than transaction guid */
{
    g_return_if_fail (fs);
    fs->m_transaction = transaction;
}

void gnc_float_split_set_memo (FloatingSplit *fs, const char *memo)
{
    g_return_if_fail (fs);
    CACHE_REPLACE (fs->m_memo, memo);
}

void gnc_float_split_set_action (FloatingSplit *fs, const char *action)
{
    g_return_if_fail (fs);
    CACHE_REPLACE (fs->m_action, action);
}

void gnc_float_split_set_reconcile_state (FloatingSplit *fs, char reconcile_state)
{
    g_return_if_fail (fs);
    fs->m_reconcile_state = reconcile_state;
}

void gnc_float_split_set_reconcile_date (FloatingSplit *fs, time64 reconcile_date)
{
    g_return_if_fail (fs);
    fs->m_reconcile_date = reconcile_date;
}

void gnc_float_split_set_amount (FloatingSplit *fs, const gnc_numeric amount)
{
    g_return_if_fail (fs);

    fs->m_amount = amount;
}

void gnc_float_split_set_value (FloatingSplit *fs, const gnc_numeric value)
{
    g_return_if_fail (fs);

    fs->m_value = value;
}

/* This function takes a split and returns a representation
   of it as a floating_split structure. Assumes the transaction is open
   for editing.
*/
FloatingSplit *gnc_split_to_float_split (Split *split)
{
    FloatingSplit *fs;

    g_return_val_if_fail (split, NULL);

    fs = g_new0 (FloatingSplit, 1);
    fs->m_split = split;
    fs->m_account = xaccSplitGetAccount (split);
    fs->m_transaction = xaccSplitGetParent (split);
    fs->m_memo = CACHE_INSERT (xaccSplitGetMemo (split));
    fs->m_action = CACHE_INSERT (xaccSplitGetAction (split));
    fs->m_reconcile_state = xaccSplitGetReconcile (split);
    fs->m_reconcile_date = xaccSplitGetDateReconciled (split);
    fs->m_amount = xaccSplitGetAmount (split);
    fs->m_value = xaccSplitGetValue (split);

    return fs;
}

/* Copy a temporary split representation onto a real split.
   If possible, insert the split into the account of the
   split representation. Not all values are copied. The reconcile
   status and date are not copied. The split's guid is,
   of course, unchanged.
*/
void gnc_float_split_to_split (const FloatingSplit *fs, Split *split)
{
    g_return_if_fail(split);

    if (fs->m_memo)
        xaccSplitSetMemo (split, fs->m_memo);
    if (fs->m_action)
        xaccSplitSetAction (split, fs->m_action);
    xaccSplitSetAmount (split, fs->m_amount);
    xaccSplitSetValue (split, fs->m_value);
    if (fs->m_account)
    {
        xaccAccountBeginEdit (fs->m_account);
        xaccSplitSetAccount (split, fs->m_account);
        xaccAccountCommitEdit (fs->m_account);
    }
}

void gnc_float_split_free (FloatingSplit *fs)
{
    g_return_if_fail (fs);

    CACHE_REMOVE (fs->m_memo);
    CACHE_REMOVE (fs->m_action);
    g_free (fs);
}

/* accessors */
Transaction *gnc_float_txn_get_txn (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_txn;
}

gnc_commodity *gnc_float_txn_get_currency (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_currency;
}

time64 gnc_float_txn_get_date_entered (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, G_MAXINT64);
    return ft->m_date_entered;
}

time64 gnc_float_txn_get_date_posted (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, G_MAXINT64);
    return ft->m_date_posted;
}

const char *gnc_float_txn_get_num (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_num;
}

const char *gnc_float_txn_get_description (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_description;
}

const char *gnc_float_txn_get_notes (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_notes;
}

const char *gnc_float_txn_get_doclink (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_doclink;
}

SplitList *gnc_float_txn_get_splits (const FloatingTxn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_splits;
}

FloatingSplit *gnc_float_txn_get_float_split (const FloatingTxn *ft, guint index)
{
    g_return_val_if_fail (ft, NULL);
    g_return_val_if_fail (ft->m_splits, NULL);
    g_return_val_if_fail (index < g_list_length (ft->m_splits) , NULL);
    return g_list_nth_data (ft->m_splits, index);
}

FloatingSplit *gnc_float_txn_get_other_float_split (const FloatingTxn *ft, FloatingSplit *fs)
{
    guint other = 0;

    g_return_val_if_fail (ft, NULL);
    g_return_val_if_fail (ft->m_splits, NULL);
    g_return_val_if_fail (g_list_length (ft->m_splits) == 2 , NULL);

    if (g_list_nth_data (ft->m_splits, 0) == fs)
        other = 1;

    return g_list_nth_data (ft->m_splits, other);
}

/* modifiers */
void gnc_float_txn_set_txn (FloatingTxn *ft, Transaction *txn)
{
    g_return_if_fail (ft);
    ft->m_txn = txn;
}

void gnc_float_txn_set_currency (FloatingTxn *ft, gnc_commodity *currency)
{
    g_return_if_fail (ft);
    ft->m_currency = currency;
}

void gnc_float_txn_set_date_entered (FloatingTxn *ft, time64 date_entered)
{
    g_return_if_fail (ft);
    ft->m_date_entered = date_entered;
}

void gnc_float_txn_set_date_posted (FloatingTxn *ft, time64 date_posted)
{
    g_return_if_fail (ft);
    ft->m_date_posted = date_posted;
}

void gnc_float_txn_set_num (FloatingTxn *ft, const char *num)
{
    g_return_if_fail (ft);
    CACHE_REPLACE (ft->m_num, num);
}

void gnc_float_txn_set_description (FloatingTxn *ft, const char *description)
{
    g_return_if_fail (ft);
    CACHE_REPLACE (ft->m_description, description);
}

void gnc_float_txn_set_notes (FloatingTxn *ft, const char *notes)
{
    g_return_if_fail (ft);
    CACHE_REPLACE (ft->m_notes, notes);
}

void gnc_float_txn_set_doclink (FloatingTxn *ft, const char *doclink)
{
    g_return_if_fail (ft);
    CACHE_REPLACE (ft->m_doclink, doclink);
}

void gnc_float_txn_set_splits (FloatingTxn *ft, SplitList *splits)
{
    g_return_if_fail (ft);
    ft->m_splits = splits;
}

void gnc_float_txn_append_float_split (FloatingTxn *ft, FloatingSplit *fs)
{
    g_return_if_fail (ft);
    g_return_if_fail (fs);
    ft->m_splits = g_list_append (ft->m_splits, fs);
}

/* This function takes a C transaction and returns
   a representation of it as a floating_txn. */
FloatingTxn *gnc_txn_to_float_txn (Transaction *txn, gboolean use_cut_semantics)
{
    GList *iter;

    FloatingTxn *ft = g_new0 (FloatingTxn, 1);

    ft->m_txn = txn;
    ft->m_currency = xaccTransGetCurrency (txn);
    ft->m_date_entered = xaccTransGetDateEntered (txn);
    if (use_cut_semantics)
    {
        ft->m_date_posted = xaccTransGetDate (txn);
        ft->m_num = CACHE_INSERT (xaccTransGetNum (txn));
    }
    ft->m_description = CACHE_INSERT (xaccTransGetDescription (txn));
    ft->m_notes = CACHE_INSERT (xaccTransGetNotes (txn));
    ft->m_doclink = CACHE_INSERT (xaccTransGetDocLink (txn));

    for (iter = xaccTransGetSplitList (txn); iter ; iter = iter->next)
    {
        Split *split = iter->data;
        if (split)
        {
            FloatingSplit *fs = gnc_split_to_float_split (split);
            ft->m_splits = g_list_prepend (ft->m_splits, fs);
        }
    }
    ft->m_splits = g_list_reverse (ft->m_splits);

    return ft;
}

void gnc_float_txn_to_txn (const FloatingTxn *ft, Transaction *txn, gboolean do_commit)
{
    gnc_float_txn_to_txn_swap_accounts (ft, txn, NULL, NULL, do_commit);
}

/* Copy a temporary representation of a transaction onto a real transaction.
 I f they exist the two account*s (acct1 and acct2) are used to swap accounts
 when when creating splits. */
void gnc_float_txn_to_txn_swap_accounts (const FloatingTxn *ft, Transaction *txn, Account *acct1, Account *acct2, gboolean do_commit)
{
    GList *iter;

    g_return_if_fail (ft);
    g_return_if_fail (txn);

    if (!xaccTransIsOpen (txn))
        xaccTransBeginEdit (txn);

    if (ft->m_currency)
        xaccTransSetCurrency (txn, ft->m_currency);
    if (ft->m_description)
        xaccTransSetDescription (txn, ft->m_description);
    if (ft->m_num)
        xaccTransSetNum (txn, ft->m_num);
    if (ft->m_notes)
        xaccTransSetNotes (txn, ft->m_notes);
    if (ft->m_doclink)
        xaccTransSetDocLink (txn, ft->m_doclink);
    if (ft->m_date_posted)
        xaccTransSetDatePostedSecs (txn, ft->m_date_posted);

    /* strip off the old splits */
    while (xaccTransCountSplits (txn))
        xaccSplitDestroy (xaccTransGetSplit (txn, 0));

    /* and put on the new ones! Please note they go in the *same*
       order as in the original transaction. This is important. */
    for (iter = ft->m_splits; iter; iter = iter->next)
    {
        Account *old_acc, *new_acc;
        Split *split;
        FloatingSplit *fs = iter->data;
        if (!fs)
            continue;

        split = xaccMallocSplit (xaccTransGetBook (txn));

        old_acc = fs->m_account;
        if (fs->m_account == acct1)
            new_acc = acct2;
        else if  (fs->m_account == acct2)
            new_acc = acct1;
        else
            new_acc = fs->m_account;

        fs->m_account = new_acc;
        gnc_float_split_to_split (fs, split);
        fs->m_account = old_acc;
        xaccSplitSetParent (split, txn);
    }

    /* close the transaction */
    if (do_commit)
        xaccTransCommitEdit (txn);
}

void gnc_float_txn_free (FloatingTxn *ft)
{
    g_return_if_fail (ft);

    CACHE_REMOVE (ft->m_num);
    CACHE_REMOVE (ft->m_description);
    CACHE_REMOVE (ft->m_notes);
    CACHE_REMOVE (ft->m_doclink);
    g_list_free_full (ft->m_splits, (GDestroyNotify)gnc_float_split_free);
    g_free (ft);
}
