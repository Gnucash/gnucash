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
\********************************************************************/

#include <glib.h>

#include "config.h"
#include "split-register-copy-ops-internals.h"

/* constructor */
floating_split *gnc_make_float_split (void)
{
    return g_new0(floating_split, 1);
}

/* type predicate */
gboolean is_gnc_float_split (const floating_split *fs)
{
    if (fs)
        return TRUE;
    else
        return FALSE;
}

/* accessors */
Split *gnc_float_split_get_split (const floating_split *fs)
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_split;
}

Account *gnc_float_split_get_account (const floating_split *fs) /* direct account pointer rather than account guid */
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_account;
}

Transaction *gnc_float_split_get_transaction (const floating_split *fs) /* direct transaction pointer rather than transaction guid */
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_transaction;
}

const char *gnc_float_split_get_memo (const floating_split *fs)
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_memo;
}

const char *gnc_float_split_get_action (const floating_split *fs)
{
    g_return_val_if_fail (fs, NULL);
    return fs->m_action;
}

char gnc_float_split_get_reconcile_state (const floating_split *fs)
{
    g_return_val_if_fail (fs, '\0');
    return fs->m_reconcile_state;
}

time64 gnc_float_split_get_reconcile_date (const floating_split *fs)
{
    g_return_val_if_fail (fs, G_MAXINT64);
    return fs->m_reconcile_date;
}

gnc_numeric gnc_float_split_get_amount (const floating_split *fs)
{
    g_return_val_if_fail (fs, gnc_numeric_zero());
    return fs->m_amount;
}

gnc_numeric gnc_float_split_get_value (const floating_split *fs)
{
    g_return_val_if_fail (fs, gnc_numeric_zero());
    return fs->m_value;
}


/* modifiers */
void gnc_float_split_set_split(floating_split *fs, Split *split)
{
    g_return_if_fail (fs);
    fs->m_split = split;
};

void gnc_float_split_set_account (floating_split *fs, Account *account) /* direct account pointer rather than account guid */
{
    g_return_if_fail (fs);
    fs->m_account = account;
};

void gnc_float_split_set_transaction (floating_split *fs, Transaction *transaction) /* direct transaction pointer rather than transaction guid */
{
    g_return_if_fail (fs);
    fs->m_transaction = transaction;
};

void gnc_float_split_set_memo (floating_split *fs, const char *memo)
{
    g_return_if_fail (fs);
    fs->m_memo = memo;
};

void gnc_float_split_set_action (floating_split *fs, const char *action)
{
    g_return_if_fail (fs);
    fs->m_action = action;
};

void gnc_float_split_set_reconcile_state (floating_split *fs, char reconcile_state)
{
    g_return_if_fail (fs);
    fs->m_reconcile_state = reconcile_state;
};

void gnc_float_split_set_reconcile_date (floating_split *fs, time64 reconcile_date)
{
    g_return_if_fail (fs);
    fs->m_reconcile_date = reconcile_date;
};

void gnc_float_split_set_amount (floating_split *fs, const gnc_numeric amount)
{
    g_return_if_fail (fs);

    fs->m_amount = amount;
};

void gnc_float_split_set_value (floating_split *fs, const gnc_numeric value)
{
    g_return_if_fail (fs);

    fs->m_value = value;
};

/* Scheme: gnc:split->split-scm
   This function takes a split and returns a representation
   of it as a floating_split structure. Assumes the transaction is open
   for editing.
*/
floating_split *gnc_split_to_float_split (Split *split)
{
    floating_split *fs;

    g_return_val_if_fail (split, NULL);

    fs = g_new0 (floating_split, 1);
    fs->m_split = split;
    fs->m_account = xaccSplitGetAccount (split);
    fs->m_transaction = xaccSplitGetParent (split);
    fs->m_memo = xaccSplitGetMemo (split);
    fs->m_action = xaccSplitGetAction (split);
    fs->m_reconcile_state = xaccSplitGetReconcile (split);
    fs->m_reconcile_date = xaccSplitGetDateReconciled (split);
    fs->m_amount = xaccSplitGetAmount (split);
    fs->m_value = xaccSplitGetValue (split);

    return fs;
}

/* Scheme: gnc:split-scm-onto-split
   Copy a scheme representation of a split onto a C split.
   If possible, insert the C split into the account of the
   scheme split. Not all values are copied. The reconcile
   status and date are not copied. The C split's guid is,
   of course, unchanged.
*/
void gnc_float_split_to_split (const floating_split *fs, Split *split)
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


floating_txn *gnc_make_float_txn (void)
{
    return g_new0(floating_txn, 1);
}

/* type predicate */
gboolean is_gnc_float_txn (const floating_txn *ft)
{
    if (ft)
        return TRUE;
    else
        return FALSE;
}

/* accessors */
Transaction *gnc_float_txn_get_txn (const floating_txn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_txn;
}

gnc_commodity *gnc_float_txn_get_currency (const floating_txn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_currency;
}

time64 gnc_float_txn_get_date_entered (const floating_txn *ft)
{
    g_return_val_if_fail (ft, G_MAXINT64);
    return ft->m_date_entered;
}

time64 gnc_float_txn_get_date_posted (const floating_txn *ft)
{
    g_return_val_if_fail (ft, G_MAXINT64);
    return ft->m_date_posted;
}

const char *gnc_float_txn_get_num (const floating_txn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_num;
}

const char *gnc_float_txn_get_description (const floating_txn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_description;
}

const char *gnc_float_txn_get_notes (const floating_txn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_notes;
}

const char *gnc_float_txn_get_association (const floating_txn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_association;
}

SplitList *gnc_float_txn_get_splits (const floating_txn *ft)
{
    g_return_val_if_fail (ft, NULL);
    return ft->m_splits;
}

/* Scheme: gnc:transaction-scm-get-split-scm */
floating_split *gnc_float_txn_get_float_split (const floating_txn *ft, guint index)
{
    floating_split *fs = NULL;
    guint size = 0;

    g_return_val_if_fail (ft, NULL);
    g_return_val_if_fail (ft->m_splits, NULL);
    g_return_val_if_fail (index < g_list_length (ft->m_splits) , NULL);
    return g_list_nth_data (ft->m_splits, index);
}

/* Scheme: gnc:transaction-scm-get-other-split-scm */
floating_split *gnc_float_txn_get_other_float_split (const floating_txn *ft, floating_split *fs)
{
    guint size = 0;

    g_return_val_if_fail (ft, NULL);
    g_return_val_if_fail (ft->m_splits, NULL);
    g_return_val_if_fail (g_list_length (ft->m_splits) == 2 , NULL);

    if (g_list_nth_data (ft->m_splits, 0) == fs)
        return g_list_nth_data (ft->m_splits, 1);
    else
        return g_list_nth_data (ft->m_splits, 0);
}

/* modifiers */
void gnc_float_txn_set_txn (floating_txn *ft, Transaction *txn)
{
    g_return_if_fail (ft);
    ft->m_txn = txn;
};

void gnc_float_txn_set_currency (floating_txn *ft, gnc_commodity *currency)
{
    g_return_if_fail (ft);
    ft->m_currency = currency;
};

void gnc_float_txn_set_date_entered (floating_txn *ft, time64 date_entered)
{
    g_return_if_fail (ft);
    ft->m_date_entered = date_entered;
};

void gnc_float_txn_set_date_posted (floating_txn *ft, time64 date_posted)
{
    g_return_if_fail (ft);
    ft->m_date_posted = date_posted;
};

void gnc_float_txn_set_num (floating_txn *ft, const char *num)
{
    g_return_if_fail (ft);
    ft->m_num = num;
};

void gnc_float_txn_set_description (floating_txn *ft, const char *description)
{
    g_return_if_fail (ft);
    ft->m_description = description;
};

void gnc_float_txn_set_notes (floating_txn *ft, const char *notes)
{
    g_return_if_fail (ft);
    ft->m_notes = notes;
};

void gnc_float_txn_set_association (floating_txn *ft, const char *association)
{
    g_return_if_fail (ft);
    ft->m_association = association;
};

void gnc_float_txn_set_splits (floating_txn *ft, SplitList *splits)
{
    g_return_if_fail (ft);
    ft->m_splits = splits;
};

/* Scheme: gnc:transaction-scm-append-split-scm */
void gnc_float_txn_append_float_split (floating_txn *ft, floating_split *fs)
{
    g_return_if_fail (ft);
    g_return_if_fail (fs);
    ft->m_splits = g_list_append (ft->m_splits, fs);
}

/* Scheme: gnc:transaction->transaction-scm
   This function takes a C transaction and returns
   a representation of it as a transaction-structure. */
floating_txn *gnc_txn_to_float_txn (Transaction *txn, gboolean use_cut_semantics)
{
    GList *iter;

    floating_txn *ft = g_new0 (floating_txn, 1);

    ft->m_txn = txn;
    ft->m_currency = xaccTransGetCurrency (txn);
    ft->m_date_entered = xaccTransGetDateEntered (txn);
    if (use_cut_semantics)
    {
        ft->m_date_posted = xaccTransGetDate (txn);
        ft->m_num = xaccTransGetNum (txn);
    }
    ft->m_description = xaccTransGetDescription (txn);
    ft->m_notes = xaccTransGetNotes (txn);
    ft->m_association = xaccTransGetAssociation (txn);

    for (iter = xaccTransGetSplitList (txn); iter ; iter = iter->next)
    {
        Split *split = iter->data;
        if (split)
        {
            floating_split *fs = gnc_split_to_float_split (split);
            ft->m_splits = g_list_append (ft->m_splits, fs);
        }
    }

    return ft;
}

/* Scheme: gnc:transaction-scm-onto-transaction
   Copy a scheme representation of a transaction onto a C transaction.
   guid-mapping must be an alist, mapping guids to guids. This list is
   used to use alternate account guids when creating splits. */
void gnc_float_txn_to_txn (const floating_txn *ft, Transaction *txn, gboolean do_commit, QofBook *book)
{
    gnc_float_txn_to_txn_swap_accounts (ft, txn, NULL, NULL, do_commit, book);
}

void gnc_float_txn_to_txn_swap_accounts (const floating_txn *ft, Transaction *txn, Account *acct1, Account *acct2, gboolean do_commit, QofBook *book)
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
    if (ft->m_association)
        xaccTransSetAssociation (txn, ft->m_association);
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
        floating_split *fs = iter->data;
        if (!fs)
            continue;

        split = xaccMallocSplit (book);

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
