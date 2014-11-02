/********************************************************************\
 * cap-gains.c -- Automatically Compute Capital Gains/Losses        *
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

/** @file cap-gains.c
 *  @brief Utilities to Automatically Compute Capital Gains/Losses.
 *  @author Created by Linas Vepstas August 2003
 *  @author Copyright (c) 2003,2004 Linas Vepstas <linas@linas.org>
 *
 *  This file implements the various routines to automatically
 *  compute and handle Cap Gains/Losses resulting from trading
 *  activities.  Some of these routines might have broader
 *  applicability, for handling depreciation & etc.
 *
 *  This code is under development, and is 'beta': we think we're
 *  mostly done, and we've tested and "things work for us", but there
 *  may still be something missing, and there might still be some
 *  bugs.
 *
 * This code uses a 'gains dirty' flag: A 'dirty' flag on the source
 * split indicates that the gains transaction needs to be recomputed.
 * Another flag, the gains transaction flag, marks the split as
 * being a gains split, and that the source transaction should be
 * checked for dirtiness before returning the date, the amount, the
 * value, etc.  Finally, these flags make amount and value read-only
 * for the gains splits. (the memo is user-modifieable).
 *
 * If the amount in a split is changed, then the lot has to be recomputed.
 * This has a potential trickle-through effect on all later lots.
 * Ideally, later lots are dissolved, and recomputed.  However, some
 * lots may have been user-hand-built. These should be left alone.
 *
ToDo:
 o XXX Need to create a data-integrity scrubber, tht makes sure that
   the various flags, and pointers & etc. match. See sections marked
   with XXX below for things that might go wrong.
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>

#include "AccountP.h"
#include "Scrub2.h"
#include "Scrub3.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "cap-gains.h"
#include "gnc-engine.h"
#include "engine-helpers.h"
#include "gnc-lot.h"
#include "policy.h"
#include "policy-p.h"

static QofLogModule log_module = GNC_MOD_LOT;


/* ============================================================== */

gboolean
xaccAccountHasTrades (const Account *acc)
{
    gnc_commodity *acc_comm;
    SplitList *splits, *node;

    if (!acc) return FALSE;

    if (xaccAccountIsPriced (acc))
        return TRUE;

    acc_comm = xaccAccountGetCommodity(acc);

    splits = xaccAccountGetSplitList(acc);
    for (node = splits; node; node = node->next)
    {
        Split *s = node->data;
        Transaction *t = s->parent;
	if (s->gains == GAINS_STATUS_GAINS) continue;
        if (acc_comm != t->common_currency) return TRUE;
    }

    return FALSE;
}

/* ============================================================== */

struct find_lot_s
{
    GNCLot *lot;
    gnc_commodity *currency;
    Timespec ts;
    int (*numeric_pred)(gnc_numeric);
    gboolean (*date_pred)(Timespec e, Timespec tr);
};

static gboolean
earliest_pred (Timespec earl, Timespec tran)
{
    return ((earl.tv_sec > tran.tv_sec)  ||
            ((earl.tv_sec == tran.tv_sec) && (earl.tv_nsec > tran.tv_nsec)));
}

static gboolean
latest_pred (Timespec earl, Timespec tran)
{
    return ((earl.tv_sec < tran.tv_sec)  ||
            ((earl.tv_sec == tran.tv_sec) && (earl.tv_nsec < tran.tv_nsec)));
}

static gpointer
finder_helper (GNCLot *lot,  gpointer user_data)
{
    struct find_lot_s *els = user_data;
    Split *s;
    Transaction *trans;
    gnc_numeric bal;
    gboolean opening_is_positive, bal_is_positive;

    if (gnc_lot_is_closed (lot)) return NULL;

    s = gnc_lot_get_earliest_split (lot);
    if (s == NULL) return NULL;

    /* We want a lot whose balance is of the correct sign.  All splits
       in a lot must be the opposite sign of the opening split.  We also
       want to ignore lots that are overfull, i.e., where the balance in
       the lot is of opposite sign to the opening split in the lot. */
    if (0 == (els->numeric_pred) (s->amount)) return NULL;
    bal = gnc_lot_get_balance (lot);
    opening_is_positive = gnc_numeric_positive_p (s->amount);
    bal_is_positive = gnc_numeric_positive_p (bal);
    if (opening_is_positive != bal_is_positive) return NULL;

    trans = s->parent;
    if (els->currency &&
            (FALSE == gnc_commodity_equiv (els->currency,
                                           trans->common_currency)))
    {
        return NULL;
    }

    if (els->date_pred (els->ts, trans->date_posted))
    {
        els->ts = trans->date_posted;
        els->lot = lot;
    }

    return NULL;
}

static inline GNCLot *
xaccAccountFindOpenLot (Account *acc, gnc_numeric sign,
                        gnc_commodity *currency,
                        gint64 guess,
                        gboolean (*date_pred)(Timespec, Timespec))
{
    struct find_lot_s es;

    es.lot = NULL;
    es.currency = currency;
    es.ts.tv_sec = guess;
    es.ts.tv_nsec = 0;
    es.date_pred = date_pred;

    if (gnc_numeric_positive_p(sign)) es.numeric_pred = gnc_numeric_negative_p;
    else es.numeric_pred = gnc_numeric_positive_p;

    xaccAccountForEachLot (acc, finder_helper, &es);
    return es.lot;
}

GNCLot *
xaccAccountFindEarliestOpenLot (Account *acc, gnc_numeric sign,
                                gnc_commodity *currency)
{
    GNCLot *lot;
    ENTER (" sign=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, sign.num,
           sign.denom);

    lot = xaccAccountFindOpenLot (acc, sign, currency,
                                  G_MAXINT64, earliest_pred);
    LEAVE ("found lot=%p %s baln=%s", lot, gnc_lot_get_title (lot),
           gnc_num_dbg_to_string(gnc_lot_get_balance(lot)));
    return lot;
}

GNCLot *
xaccAccountFindLatestOpenLot (Account *acc, gnc_numeric sign,
                              gnc_commodity *currency)
{
    GNCLot *lot;
    ENTER (" sign=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT,
           sign.num, sign.denom);

    lot = xaccAccountFindOpenLot (acc, sign, currency,
                                  G_MININT64, latest_pred);
    LEAVE ("found lot=%p %s", lot, gnc_lot_get_title (lot));
    return lot;
}

/* ============================================================== */

Split *
xaccSplitAssignToLot (Split *split, GNCLot *lot)
{
    Account *acc;
    gnc_numeric baln;
    int cmp;
    gboolean baln_is_positive, amt_is_positive;

    if (!lot) return split;
    if (!split) return NULL;

    /* If this split already belongs to a lot, we are done. */
    if (split->lot) return NULL;

    /* Anomolous situation; except for voided transactions,
     * we don't expect to see splits with no amount ..
     * unless they're gains splits, and we shouldn't see those.
     */
    if (gnc_numeric_zero_p (split->amount))
    {
        if (xaccTransGetVoidStatus(split->parent)) return NULL;

        PWARN ("split with zero amount; value=%s gflag=%x gsplit=%p",
               gnc_num_dbg_to_string (split->amount),
               split->gains,
               split->gains_split);
        if (split->gains_split)
        {
            PWARN ("gains amt=%s value=%s",
                   gnc_num_dbg_to_string (split->gains_split->amount),
                   gnc_num_dbg_to_string (split->gains_split->value));
        }
        return NULL;
    }

    /* If the lot is closed, we can't add anything to it */
    baln = gnc_lot_get_balance (lot);
    if (gnc_lot_is_closed (lot)) return split;

    /* If the lot balance is zero, but the lot is open, then
     * the lot is empty. Unconditionally add the split. */
    if (gnc_numeric_zero_p (baln))
    {
        acc = split->acc;
        xaccAccountBeginEdit (acc);
        gnc_lot_add_split (lot, split);
        PINFO ("added split to empty lot, new lot baln=%s (%s)",
               gnc_num_dbg_to_string (gnc_lot_get_balance(lot)),
               gnc_lot_get_title (lot));
        xaccAccountCommitEdit (acc);
        return NULL;
    }

    /* If the sign of the split is the same as the sign of the lot,
     * add the split, but complain about it ... none of the currently
     * implemented accounting policies should be giving us splits
     * that make lots larger.  One a lot is open, the FIFO/LIFO
     * policies should be working only to make the lot smaller.
     * We can remove teh warning emssage come the day we have
     * fancier policies.
     */
    baln_is_positive = gnc_numeric_positive_p (baln);
    amt_is_positive = gnc_numeric_positive_p (split->amount);
    if ((baln_is_positive && amt_is_positive) ||
            ((!baln_is_positive) && (!amt_is_positive)))
    {
        PWARN ("accounting policy gave us split that enlarges the lot!\n"
               "old lot baln=%s split amt=%s lot=%s",
               gnc_num_dbg_to_string (gnc_lot_get_balance(lot)),
               gnc_num_dbg_to_string (split->amount),
               gnc_lot_get_title (lot));

        acc = split->acc;
        xaccAccountBeginEdit (acc);
        gnc_lot_add_split (lot, split);
        xaccAccountCommitEdit (acc);
        return NULL;
    }

    /* If adding the split would make the lot balance change sign,
     * then we split the split into two pieces: one piece that will
     * bring the lot balance to zero, and another to be dealt with
     * later.  */
    cmp = gnc_numeric_compare (gnc_numeric_abs(split->amount),
                               gnc_numeric_abs(baln));

    PINFO ("found open lot with baln=%s (%s)", gnc_num_dbg_to_string (baln),
           gnc_lot_get_title (lot));

    /* cmp == -1 if amt < baln, ==0 if amt==baln */
    if (0 >= cmp)
    {
        acc = split->acc;
        xaccAccountBeginEdit (acc);
        gnc_lot_add_split (lot, split);
        PINFO ("simple added split to lot, new lot baln=%s",
               gnc_num_dbg_to_string (gnc_lot_get_balance(lot)));
        xaccAccountCommitEdit (acc);
        return NULL;
    }

    /* If we are here, then (cmp == +1 iff (amt > baln)) and we need
     * to split up the split into pieces. Do it. */
    {
        time64 now = gnc_time (NULL);
        Split * new_split;
        gnc_numeric amt_a, amt_b, amt_tot;
        gnc_numeric val_a, val_b, val_tot;
        gnc_numeric frac;
        Transaction *trans;
        Timespec ts;

        acc = split->acc;
        xaccAccountBeginEdit (acc);
        trans = split->parent;
        xaccTransBeginEdit (trans);

        amt_tot = split->amount;
        amt_a = gnc_numeric_neg (baln);
        amt_b = gnc_numeric_sub_fixed (amt_tot, amt_a);

        PINFO ("++++++++++++++ splitting split=%p into amt = %s + %s",
               split,
               gnc_num_dbg_to_string(amt_a),
               gnc_num_dbg_to_string(amt_b) );

        /* Compute the value so that it holds in the same proportion:
         * i.e. so that (amt_a / amt_tot) = (val_a / val_tot)
         */
        val_tot = split->value;
        frac = gnc_numeric_div (amt_a, amt_tot,
                                GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        val_a = gnc_numeric_mul (frac, val_tot,
                                 gnc_numeric_denom(val_tot),
                                 GNC_HOW_RND_ROUND_HALF_UP | GNC_HOW_DENOM_EXACT);

        val_b = gnc_numeric_sub_fixed (val_tot, val_a);
        if (gnc_numeric_check(val_a))
        {
            PERR("Numeric overflow\n"
                 "Acct=%s Txn=%s\n"
                 "\tval_tot=%s amt_a=%s amt_tot=%s\n",
                 xaccAccountGetName(acc),
                 xaccTransGetDescription(trans),
                 gnc_num_dbg_to_string(val_tot),
                 gnc_num_dbg_to_string(amt_a),
                 gnc_num_dbg_to_string(amt_tot));
        }

        if (gnc_numeric_zero_p(val_a) || gnc_numeric_zero_p(val_b))
        {
            PERR ("Failed to split into two!");
        }

        PINFO ("split value is = %s = %s + %s",
               gnc_num_dbg_to_string(val_tot),
               gnc_num_dbg_to_string(val_a),
               gnc_num_dbg_to_string(val_b) );

        xaccSplitSetAmount (split, amt_a);
        xaccSplitSetValue (split, val_a);

        /* Adding this split will have the effect of closing this lot,
         * because the new balance should be precisely zero. */
        gnc_lot_add_split (lot, split);

        /* Put the remainder of the balance into a new split,
         * which is in other respects just a clone of this one. */
        new_split = xaccMallocSplit (qof_instance_get_book(acc));

        /* Copy most of the split attributes */
        xaccSplitSetMemo (new_split, xaccSplitGetMemo (split));
        /* Set split-action with gnc_set_num_action which is the same as
         * xaccSplitSetAction with these arguments; use gnc_get_num_action to get
         * split-action which is the same as xaccSplitGetAction */
        gnc_set_num_action(NULL, new_split, NULL, gnc_get_num_action(NULL, split));
        xaccSplitSetReconcile (new_split, xaccSplitGetReconcile (split));
        ts = xaccSplitRetDateReconciledTS (split);
        xaccSplitSetDateReconciledTS (new_split, &ts);

        /* Set the lot-split and peer_guid properties on the two
         * splits to indicate that they're linked. 
         */
        qof_instance_set (QOF_INSTANCE (split),
                          "lot-split", now,
                          "peer_guid", xaccSplitGetGUID (new_split),
                          NULL);

        qof_instance_set (QOF_INSTANCE (new_split),
                          "lot-split", now,
                          "peer_guid", xaccSplitGetGUID (split),
                          NULL);

        xaccAccountInsertSplit (acc, new_split);
        xaccTransAppendSplit (trans, new_split);
        /* Set the amount and value after the split is in the transaction
           so it can find the correct denominator to use.  Otherwise it
           uses 100000 which may cause an overflow in some of the tests
           in test-period */
        xaccSplitSetAmount (new_split, amt_b);
        xaccSplitSetValue (new_split, val_b);
        xaccTransCommitEdit (trans);
        xaccAccountCommitEdit (acc);
        return new_split;
    }
}

/* ============================================================== */

/* Accounting-policy callback.  Given an account and an amount,
 * this routine should return a lot.  By implementing this as
 * a callback, we can 'easily' add other accounting policies.
 */
gboolean
xaccSplitAssign (Split *split)
{
    Account *acc;
    gboolean splits_split_up = FALSE;
    GNCLot *lot;
    GNCPolicy *pcy;

    if (!split) return FALSE;

    /* If this split already belongs to a lot or the account doesn't
     * have lots, we are done.
     */
    if (split->lot) return FALSE;
    g_assert (split->gains == GAINS_STATUS_UNKNOWN ||
	      (split->gains & GAINS_STATUS_GAINS) == FALSE);
    acc = split->acc;
    if (!xaccAccountHasTrades (acc))
        return FALSE;
    if (gnc_numeric_zero_p (split->amount))
        return FALSE;

    ENTER ("(split=%p)", split);

    pcy = gnc_account_get_policy(acc);
    xaccAccountBeginEdit (acc);

    /* If we are here, this split does not belong to any lot.
     * We ask the policy for a lot to assign it to.  This
     * block is written in the form of a while loop, since we
     * may have to bust a split across several lots.
     */
    while (split)
    {
        PINFO ("have split %p amount=%s", split,
               gnc_num_dbg_to_string (split->amount));
        split->gains |= GAINS_STATUS_VDIRTY;
        lot = pcy->PolicyGetLot (pcy, split);
        if (!lot)
        {
            lot = gnc_lot_make_default (acc);
            PINFO ("start new lot (%s)", gnc_lot_get_title(lot));
        }
        split = xaccSplitAssignToLot (split, lot);
        if (split) splits_split_up = TRUE;
    }
    xaccAccountCommitEdit (acc);

    LEAVE (" split_up=%d", splits_split_up);
    return splits_split_up;
}

/* ============================================================== */

Split *
xaccSplitGetCapGainsSplit (const Split *split)
{
    GncGUID *gains_guid;
    Split *gains_split;

    if (!split) return NULL;

    qof_instance_get (QOF_INSTANCE (split),
                      "gains-split", &gains_guid,
                      NULL);
    if (!gains_guid) return NULL;

    /* Both splits will be in the same collection, so search there. */
    gains_split = (Split*) qof_collection_lookup_entity (
                      qof_instance_get_collection(split), gains_guid);
    PINFO ("split=%p has gains-split=%p", split, gains_split);
    return gains_split;
}

/* ============================================================== */

Split *
xaccSplitGetGainsSourceSplit (const Split *split)
{
    GncGUID *source_guid;
    Split *source_split;

    if (!split) return NULL;

    qof_instance_get (QOF_INSTANCE (split),
                      "gains-source", &source_guid,
                      NULL);
    if (!source_guid) return NULL;

    /* Both splits will be in the same collection, so search there. */
    source_split = (Split*) qof_collection_lookup_entity(
                       qof_instance_get_collection(split), source_guid);
    PINFO ("split=%p has source-split=%p", split, source_split);
    return source_split;
}

/* ============================================================== */

void
xaccSplitComputeCapGains(Split *split, Account *gain_acc)
{
    SplitList *node;
    GNCLot *lot;
    GNCPolicy *pcy;
    gnc_commodity *currency = NULL;
    gnc_numeric zero = gnc_numeric_zero();
    gnc_numeric value = zero;
    gnc_numeric frac;
    gnc_numeric opening_amount, opening_value;
    gnc_numeric lot_amount, lot_value;
    gnc_commodity *opening_currency;

    if (!split) return;
    lot = split->lot;
    if (!lot) return;
    pcy = gnc_account_get_policy(gnc_lot_get_account(lot));
    currency = split->parent->common_currency;

    ENTER ("(split=%p gains=%p status=0x%x lot=%s)", split,
           split->gains_split, split->gains, gnc_lot_get_title(lot));

    /* Make sure the status flags and pointers are initialized */
    xaccSplitDetermineGainStatus(split);

    /* Not possible to have gains if the transaction currency and
     * account commodity are identical. */
    if (gnc_commodity_equal (currency,
                             xaccAccountGetCommodity(split->acc)))
    {
        LEAVE ("Currency transfer, gains not possible, returning.");
        return;
    }

    if (pcy->PolicyIsOpeningSplit (pcy, lot, split))
    {
#if MOVE_THIS_TO_A_DATA_INTEGRITY_SCRUBBER
        /* Check to make sure that this opening split doesn't
         * have a cap-gain transaction associated with it.
         * If it does, that's wrong, and we ruthlessly destroy it.
         * XXX Don't do this, it leads to infinite loops.
         * We need to scrub out errors like this elsewhere!
         */
        if (xaccSplitGetCapGainsSplit (split))
        {
            Split *gains_split = xaccSplitGetCapGainsSplit(split);
            Transaction *trans = gains_split->parent;
            PERR ("Opening Split must not have cap gains!!\n");

            xaccTransBeginEdit (trans);
            xaccTransDestroy (trans);
            xaccTransCommitEdit (trans);
        }
#endif
        LEAVE ("Lot opening split, returning.");
        return;
    }

    if (g_strcmp0 ("stock-split", xaccSplitGetType (split)) == 0)
    {
        LEAVE ("Stock split split, returning.");
        return;
    }

    if (GAINS_STATUS_GAINS & split->gains)
    {
        Split *s;
        PINFO ("split is a gains recording split, switch over");
        /* If this is the split that records the gains, then work with
         * the split that generates the gains.
         */
        /* split = xaccSplitGetCapGainsSplit (split); */
        s = split->gains_split;

        /* This should never be NULL, and if it is, and its matching
         * parent can't be found, then its a bug, and we should be
         * discarding this split.   But ... for now .. return.
         * XXX move appropriate actions to a 'scrub' routine'
         */
        if (!s)
        {
            PERR ("Bad gains-split pointer! .. trying to recover.");
            split->gains_split = xaccSplitGetCapGainsSplit (split);
            s = split->gains_split;
            if (!s) return;
#if MOVE_THIS_TO_A_DATA_INTEGRITY_SCRUBBER
            xaccTransDestroy (trans);
#endif
        }
        split = s;
    }

    /* Note: if the value of the 'opening' split(s) has changed,
     * then the cap gains are changed. So we need to check not
     * only if this split is dirty, but also the lot-opening splits. */
    for (node = gnc_lot_get_split_list(lot); node; node = node->next)
    {
        Split *s = node->data;
        if (pcy->PolicyIsOpeningSplit(pcy, lot, s))
        {
            if (GAINS_STATUS_UNKNOWN == s->gains) xaccSplitDetermineGainStatus (s);
            if (s->gains & GAINS_STATUS_VDIRTY)
            {
                /* Force a recompute to occur */
                split->gains |= GAINS_STATUS_VDIRTY;
                break;
            }
        }
    }

    /* If it doesn't look like this split is 'dirty', then there's
     * nothing to do. Just return. */
    if ((FALSE == (split->gains & GAINS_STATUS_A_VDIRTY))  &&
            (split->gains_split) &&
            (FALSE == (split->gains_split->gains & GAINS_STATUS_A_VDIRTY)))
    {
        LEAVE ("split not dirty, returning");
        return;
    }

    /* Yow! If amount is zero, there's nothing to do! Amount-zero splits
     * may exist if users attempted to manually record gains. */
    if (gnc_numeric_zero_p (split->amount)) return;

    /* If we got to here, then the split or something related is
     * 'dirty' and the gains really do need to be recomputed.
     * So start working things. */

    /* Get the amount and value in this lot at the time of this transaction. */
    gnc_lot_get_balance_before (lot, split, &lot_amount, &lot_value);

    pcy->PolicyGetLotOpening (pcy, lot, &opening_amount, &opening_value,
                              &opening_currency);

    /* Check to make sure the lot-opening currency and this split
     * use the same currency */
    if (FALSE == gnc_commodity_equiv (currency, opening_currency))
    {
        /* OK, the purchase and the sale were made in different currencies.
         * I don't know how to compute cap gains for that.  This is not
         * an error. Just punt, silently.
         */
        LEAVE ("Can't compute gains, mismatched commodities!");
        return;
    }

    /* Opening amount should be larger (or equal) to current split,
     * and it should be of the opposite sign.
     * XXX This should really be a part of a scrub routine that
     * cleans up the lot, before we get at it!
     */
    if (0 > gnc_numeric_compare (gnc_numeric_abs(lot_amount),
                                 gnc_numeric_abs(split->amount)))
    {
        GList *n;
        for (n = gnc_lot_get_split_list(lot); n; n = n->next)
        {
            Split *s = n->data;
            PINFO ("split amt=%s", gnc_num_dbg_to_string(s->amount));
        }
        PERR ("Malformed Lot \"%s\"! (too thin!) "
              "opening amt=%s split amt=%s baln=%s",
              gnc_lot_get_title (lot),
              gnc_num_dbg_to_string (lot_amount),
              gnc_num_dbg_to_string (split->amount),
              gnc_num_dbg_to_string (gnc_lot_get_balance(lot)));
        return;
    }
    if ( (gnc_numeric_negative_p(lot_amount) ||
            gnc_numeric_positive_p(split->amount)) &&
            (gnc_numeric_positive_p(lot_amount) ||
             gnc_numeric_negative_p(split->amount)))
    {
        GList *n;
        for (n = gnc_lot_get_split_list(lot); n; n = n->next)
        {
            Split *s = n->data;
            PINFO ("split amt=%s", gnc_num_dbg_to_string(s->amount));
        }
        PERR ("Malformed Lot \"%s\"! (too fat!) "
              "opening amt=%s split amt=%s baln=%s",
              gnc_lot_get_title (lot),
              gnc_num_dbg_to_string (lot_amount),
              gnc_num_dbg_to_string (split->amount),
              gnc_num_dbg_to_string (gnc_lot_get_balance(lot)));
        return;
    }

    /* The cap gains is the difference between the basis prior to the
     * current split, and the current split, pro-rated for an equal
     * amount of shares.
     * i.e. purchase_price = lot_value / lot_amount
     * cost_basis = purchase_price * current_split_amount
     * cap_gain = current_split_value - cost_basis
     */
    /* Fraction of the lot that this split represents: */
    frac = gnc_numeric_div (split->amount, lot_amount,
                            GNC_DENOM_AUTO,
                            GNC_HOW_DENOM_REDUCE);
    /* Basis for this split: */
    value = gnc_numeric_mul (frac, lot_value,
                             gnc_numeric_denom(opening_value),
                             GNC_HOW_DENOM_EXACT | GNC_HOW_RND_ROUND_HALF_UP);
    /* Capital gain for this split: */
    value = gnc_numeric_sub (value, split->value,
                             GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);
    PINFO ("Open amt=%s val=%s;  split amt=%s val=%s; gains=%s\n",
           gnc_num_dbg_to_string (lot_amount),
           gnc_num_dbg_to_string (lot_value),
           gnc_num_dbg_to_string (split->amount),
           gnc_num_dbg_to_string (split->value),
           gnc_num_dbg_to_string (value));
    if (gnc_numeric_check (value))
    {
        PERR ("Numeric overflow during gains calculation\n"
              "Acct=%s Txn=%s\n"
              "\tOpen amt=%s val=%s\n\tsplit amt=%s val=%s\n\tgains=%s\n",
              xaccAccountGetName(split->acc),
              xaccTransGetDescription(split->parent),
              gnc_num_dbg_to_string (lot_amount),
              gnc_num_dbg_to_string (lot_value),
              gnc_num_dbg_to_string (split->amount),
              gnc_num_dbg_to_string (split->value),
              gnc_num_dbg_to_string (value));
        return;
    }

    /* Are the cap gains zero?  If not, add a balancing transaction.
     * As per design doc lots.txt: the transaction has two splits,
     * with equal & opposite values.  The amt of one iz zero (so as
     * not to upset the lot balance), the amt of the other is the same
     * as its value (its the realized gain/loss).
     */
    if (FALSE == gnc_numeric_zero_p (value))
    {
        Transaction *trans;
        Split *lot_split, *gain_split;
        Timespec ts;
        gboolean new_gain_split;
        gnc_numeric negvalue = gnc_numeric_neg (value);

        /* See if there already is an associated gains transaction.
         * If there is, adjust its value as appropriate. Else, create
         * a new gains transaction.
         */
        /* lot_split = xaccSplitGetCapGainsSplit (split);  */
        lot_split = split->gains_split;

        if (NULL == lot_split)
        {
            Account *lot_acc = gnc_lot_get_account(lot);
            QofBook *book = qof_instance_get_book(lot_acc);
            Transaction *base_txn = xaccSplitGetParent (split);

            new_gain_split = TRUE;

            lot_split = xaccMallocSplit (book);
            gain_split = xaccMallocSplit (book);

            /* Check to make sure the gains account currency matches. */
            if ((NULL == gain_acc) ||
                    (FALSE == gnc_commodity_equiv (currency,
                                                   xaccAccountGetCommodity(gain_acc))))
            {
                gain_acc = xaccAccountGainsAccount (lot_acc, currency);
            }

            xaccAccountBeginEdit (gain_acc);
            xaccAccountInsertSplit (gain_acc, gain_split);
            xaccAccountCommitEdit (gain_acc);

            xaccAccountBeginEdit (lot_acc);
            xaccAccountInsertSplit (lot_acc, lot_split);
            xaccAccountCommitEdit (lot_acc);

            trans = xaccMallocTransaction (book);

            xaccTransBeginEdit (trans);
            xaccTransSetCurrency (trans, currency);
            xaccTransSetDescription (trans, _("Realized Gain/Loss"));

            xaccTransAppendSplit (trans, lot_split);
            xaccTransAppendSplit (trans, gain_split);

            xaccSplitSetMemo (lot_split, _("Realized Gain/Loss"));
            xaccSplitSetMemo (gain_split, _("Realized Gain/Loss"));

            /* For the new transaction, set the split properties indicating
             * that this is the gains transaction that corresponds
             * to the gains source.
             */
            xaccTransBeginEdit (base_txn);
            qof_instance_set (QOF_INSTANCE (split),
                              "gains-split", xaccSplitGetGUID (lot_split),
                              NULL);
            xaccTransCommitEdit (base_txn);
            qof_instance_set (QOF_INSTANCE (lot_split),
                              "gains-source", xaccSplitGetGUID (split),
                              NULL);

        }
        else
        {
            trans = lot_split->parent;
            gain_split = xaccSplitGetOtherSplit (lot_split);

            /* If the gains transaction has been edited so that it no longer has
               just two splits, ignore it and assume it's still correct. */
            if (!gain_split)
            {
                new_gain_split = FALSE;
            }
            /* If the gain is already recorded corectly do nothing.  This is
             * more than just an optimization since this may be called during
             * gnc_book_partition_txn and depending on the order in which things
             * happen some splits may be in the wrong book at that time. */
            else if (split->gains_split == lot_split &&
                     lot_split->gains_split == split &&
                     gain_split->gains_split == split &&
                     gnc_numeric_equal (xaccSplitGetValue (lot_split), value) &&
                     gnc_numeric_zero_p (xaccSplitGetAmount (lot_split)) &&
                     gnc_numeric_equal (xaccSplitGetValue (gain_split), negvalue) &&
                     gnc_numeric_equal (xaccSplitGetAmount (gain_split), negvalue))
            {
                new_gain_split = FALSE;
            }
            else
            {
                new_gain_split = TRUE;
                xaccTransBeginEdit (trans);

                /* Make sure the existing gains trans has the correct currency,
                 * just in case someone screwed with it! */
                if (FALSE == gnc_commodity_equiv(currency, trans->common_currency))
                {
                    PWARN ("Resetting the transaction currency!");
                    xaccTransSetCurrency (trans, currency);
                }
            }
        }

        if (new_gain_split)
        {
            /* Common to both */
            ts = xaccTransRetDatePostedTS (split->parent);
            xaccTransSetDatePostedTS (trans, &ts);
            xaccTransSetDateEnteredSecs (trans, gnc_time (NULL));

            xaccSplitSetAmount (lot_split, zero);
            xaccSplitSetValue (lot_split, value);

            xaccSplitSetAmount (gain_split, negvalue);
            xaccSplitSetValue (gain_split, negvalue);

            /* Some short-cuts to help avoid the above property lookup. */
            split->gains = GAINS_STATUS_CLEAN;
            split->gains_split = lot_split;
            lot_split->gains = GAINS_STATUS_GAINS;
            lot_split->gains_split = split;
            gain_split->gains = GAINS_STATUS_GAINS;
            gain_split->gains_split = split;

            /* Do this last since it may generate an event that will call us
               recursively. */
            gnc_lot_add_split (lot, lot_split);

            xaccTransCommitEdit (trans);
        }
    }
    LEAVE ("(lot=%s)", gnc_lot_get_title(lot));
}

/* ============================================================== */

gnc_numeric
xaccSplitGetCapGains(Split * split)
{
    if (!split) return gnc_numeric_zero();
    ENTER("(split=%p)", split);

    if (GAINS_STATUS_UNKNOWN == split->gains)
        xaccSplitDetermineGainStatus(split);
    if ((split->gains & GAINS_STATUS_A_VDIRTY) ||
            (split->gains_split &&
             (split->gains_split->gains & GAINS_STATUS_A_VDIRTY)))
    {
        xaccSplitComputeCapGains (split, NULL);
    }

    /* If this is the source split, get the gains from the one
     * that records the gains.  If this already is the gains split,
     * its a no-op. */
    if (!(GAINS_STATUS_GAINS & split->gains))
    {
        /* split = xaccSplitGetCapGainsSplit (split); */
        split = split->gains_split;
    }

    LEAVE("(split=%p)", split);
    if (!split) return gnc_numeric_zero();

    return split->value;
}

/* ============================================================== */

void
xaccLotComputeCapGains (GNCLot *lot, Account *gain_acc)
{
    SplitList *node;
    GNCPolicy *pcy;
    gboolean is_dirty = FALSE;

    /* Note: if the value of the 'opening' split(s) has changed,
     * then the cap gains are changed. To capture this, we need
     * to mark all splits dirty if the opening splits are dirty. */

    ENTER("(lot=%p)", lot);
    pcy = gnc_account_get_policy(gnc_lot_get_account(lot));
    for (node = gnc_lot_get_split_list(lot); node; node = node->next)
    {
        Split *s = node->data;
        if (pcy->PolicyIsOpeningSplit(pcy, lot, s))
        {
            if (GAINS_STATUS_UNKNOWN == s->gains)
                xaccSplitDetermineGainStatus(s);
            if (s->gains & GAINS_STATUS_VDIRTY)
            {
                is_dirty = TRUE;
                s->gains &= ~GAINS_STATUS_VDIRTY;
            }
        }
    }

    if (is_dirty)
    {
        for (node = gnc_lot_get_split_list(lot); node; node = node->next)
        {
            Split *s = node->data;
            s->gains |= GAINS_STATUS_VDIRTY;
        }
    }

    for (node = gnc_lot_get_split_list(lot); node; node = node->next)
    {
        Split *s = node->data;
        xaccSplitComputeCapGains (s, gain_acc);
    }
    LEAVE("(lot=%p)", lot);
}

/* =========================== END OF FILE ======================= */
