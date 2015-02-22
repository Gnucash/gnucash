/********************************************************************\
 * ScrubBusiness.h -- Cleanup functions for the business objects.   *
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

/** @file ScrubBusiness.h
 *  @brief Cleanup functions for business objects
 *  @author Created by Geert Janssens August 2014
 *  @author Copyright (c) 2014 Geert Janssens <geert@kobaltwit.be>
 *
 * Provides the high-level API for checking and repairing ('scrubbing
 * clean') the various data objects used by the business functions.*/

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>

#include "gnc-engine.h"
#include "gnc-lot.h"
#include "policy-p.h"
#include "Account.h"
#include "gncInvoice.h"
#include "Scrub2.h"
#include "ScrubBusiness.h"
#include "Transaction.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.engine.scrub"

static QofLogModule log_module = G_LOG_DOMAIN;

// A helper function that takes two splits. If the splits are  of opposite sign
// it reduces the biggest split to have the same value (but with opposite sign)
// of the smaller split.
// To make sure everything still continues to balance in addition a "remainder" split
// will be created that will be added to the same lot and transaction as the biggest
// split.
// The opposite sign restriction is because that's the only scenario that makes sense
// in the context of scrubbing business lots below.
// If we created new splits, return TRUE, otherwise FALSE
static gboolean reduce_biggest_split (Split *splitA, Split *splitB)
{
    gnc_numeric valA = xaccSplitGetValue (splitA);
    gnc_numeric valB = xaccSplitGetValue (splitB);

    if (gnc_numeric_compare (gnc_numeric_abs (valA), gnc_numeric_abs (valB)) >= 0)
        return gncOwnerReduceSplitTo (splitA, gnc_numeric_neg (valB));
    else
        return gncOwnerReduceSplitTo (splitB, gnc_numeric_neg (valA));
}

// Attempt to eliminate or reduce the lot link splits (ll_*_split)
// between from_lot and to_lot. To do so this function will attempt
// to move a payment split from from_lot to to_lot in order to
// balance the lot link split that will be deleted.
// To ensure everything remains balanced at most
// min (val-ll-*-split, val-pay-split) (in absolute values) can be moved.
// If any split involved has a larger value, it will be split in two
// and only the part matching the other splits' value will be used.
// The leftover splits are kept in the respective transactions/lots.
// A future scrub action can still act on those if needed.
//
// Note that this function assumes that ll_from_split and ll_to_split are
// of opposite sign. The calling function should check this.

static gboolean
scrub_other_link (GNCLot *from_lot, Split *ll_from_split,
                  GNCLot *to_lot,   Split *ll_to_split)
{
    Split *real_from_split; // This refers to the split in the payment lot representing the payment itself
    gboolean modified = FALSE;
    gnc_numeric real_from_val;
    gnc_numeric from_val = xaccSplitGetValue (ll_from_split);
    gnc_numeric to_val = xaccSplitGetValue (ll_to_split);
    Transaction *ll_txn = xaccSplitGetParent (ll_to_split);

    // Per iteration we can only scrub at most min (val-doc-split, val-pay-split)
    // So set the ceiling for finding a potential offsetting split in the lot
    if (gnc_numeric_compare (gnc_numeric_abs (from_val), gnc_numeric_abs (to_val)) >= 0)
        from_val = gnc_numeric_neg (to_val);

    // Next we have to find the original payment split so we can
    // add (part of) it to the document lot
    real_from_split = gncOwnerFindOffsettingSplit (from_lot, from_val);
    if (!real_from_split)
        return FALSE; // No usable split in the payment lot

    // We now have found 3 splits involved in the scrub action:
    // 2 lot link splits which we want to reduce
    // 1 other split to move into the original lot instead of the lot link split
    // As said only value of the split can be offset.
    // So split the bigger ones in two if needed and continue with equal valued splits only
    // The remainder is added to the lot link transaction and the lot to keep everything balanced
    // and will be processed in a future iteration
    modified = reduce_biggest_split (ll_from_split, ll_to_split);
    modified |= reduce_biggest_split (real_from_split, ll_from_split);
    modified |= reduce_biggest_split (ll_from_split, ll_to_split);

    // At this point ll_to_split and real_from_split should have the same value
    // If not, flag a warning and skip to the next iteration
    to_val        = xaccSplitGetValue (ll_to_split);
    from_val      = xaccSplitGetValue (ll_from_split);
    real_from_val = xaccSplitGetValue (real_from_split);
    if (!gnc_numeric_equal (real_from_val, to_val))
    {
        // This is unexpected - write a warning message and skip this split
        PWARN("real_from_val (%s) and to_val (%s) differ. "
              "This is unexpected! Skip scrubbing of real_from_split %p against ll_to_split %p.",
              gnc_numeric_to_string (real_from_val), // gnc_numeric_denom (real_from_val),
              gnc_numeric_to_string (to_val), // gnc_numeric_denom (to_val),
              real_from_split, ll_to_split);
        return modified;
    }

    // Now do the actual split dance
    // - move real payment split to doc lot
    // - delete both lot link splits from the lot link transaction
    gnc_lot_add_split (to_lot, real_from_split);
    xaccTransBeginEdit (ll_txn);
    xaccSplitDestroy (ll_to_split);
    xaccSplitDestroy (ll_from_split);
    xaccTransCommitEdit (ll_txn);

    // Cleanup the lots
    xaccScrubMergeLotSubSplits (to_lot, FALSE);
    xaccScrubMergeLotSubSplits (from_lot, FALSE);

    return TRUE; // We did change splits/transactions/lots...
}

static gboolean
gncScrubLotLinks (GNCLot *scrub_lot)
{
    gboolean modified = FALSE, restart_needed = FALSE;
    SplitList *sls_iter = NULL;

scrub_start:
    restart_needed = FALSE;

    // Iterate over all splits in the lot
    for (sls_iter = gnc_lot_get_split_list (scrub_lot); sls_iter; sls_iter = sls_iter->next)
    {
        Split *sl_split = sls_iter->data;
        Transaction *ll_txn = NULL; // ll_txn = "Lot Link Transaction"
        SplitList *lts_iter = NULL;

        if (!sl_split)
            continue; // next scrub lot split

        // Only lot link transactions need to be scrubbed
        ll_txn = xaccSplitGetParent (sl_split);

        if (!ll_txn)
        {
            // Ooops - the split doesn't belong to any transaction !
            // This is not expected so issue a warning and continue with next split
            PWARN("Encountered a split in a business lot that's not part of any transaction. "
                  "This is unexpected! Skipping split %p.", sl_split);
            continue;
        }

        if (xaccTransGetTxnType (ll_txn) != TXN_TYPE_LINK)
            continue; // next scrub lot split

        // Iterate over all splits in the lot link transaction
        for (lts_iter = xaccTransGetSplitList (ll_txn); lts_iter; lts_iter = lts_iter->next)
        {
            Split *ll_txn_split = lts_iter->data; // These all refer to splits in the lot link transaction
            GNCLot *remote_lot = NULL; // lot at the other end of the lot link transaction
            gboolean sl_is_doc_lot, rl_is_doc_lot;

            if (!ll_txn_split)
                continue; // next lot link transaction split

            // Skip the split in the lot we're currently scrubbing
            if (sl_split == ll_txn_split)
                continue; // next lot link transaction split

            // Only splits of opposite signed values can be scrubbed
            if (gnc_numeric_positive_p (xaccSplitGetValue (sl_split)) ==
                gnc_numeric_positive_p (xaccSplitGetValue (ll_txn_split)))
                continue; // next lot link transaction split

            // Find linked lot via split
            remote_lot = xaccSplitGetLot (ll_txn_split);
            if (!remote_lot)
            {
                // This is unexpected - write a warning message and skip this split
                PWARN("Encountered a Lot Link transaction with a split that's not in any lot. "
                      "This is unexpected! Skipping split %p from transaction %p.", ll_txn_split, ll_txn);
                continue;
            }

            sl_is_doc_lot = (gncInvoiceGetInvoiceFromLot (scrub_lot) != NULL);
            rl_is_doc_lot = (gncInvoiceGetInvoiceFromLot (remote_lot) != NULL);

            // Depending on the type of lots we're comparing, we need different actions
            // - Two document lots (an invoice and a credit note):
            //   Special treatment - look for all document lots linked via ll_txn
            //   and update the memo to be of more use to the uses.
            // - Two payment lots:
            //   (Part of) the link will be eliminated and instead (part of)
            //   one payment will be added to the other lot to keep the balance.
            //   If the payments are not equal in abs value part of the bigger payment
            //   will be moved to the smaller payment's lot.
            // - A document and a payment lot:
            //   (Part of) the link will be eliminated and instead (part of) the real
            //   payment will be added to the document lot to handle the payment.
            if (sl_is_doc_lot && rl_is_doc_lot)
                gncOwnerSetLotLinkMemo (ll_txn);
            else if (!sl_is_doc_lot && !rl_is_doc_lot)
            {
                gint cmp = gnc_numeric_compare (gnc_numeric_abs (xaccSplitGetValue (sl_split)),
                                                gnc_numeric_abs (xaccSplitGetValue (ll_txn_split)));
                if (cmp >= 0)
                    restart_needed = scrub_other_link (scrub_lot, sl_split, remote_lot, ll_txn_split);
                else
                    restart_needed = scrub_other_link (remote_lot, ll_txn_split, scrub_lot, sl_split);
            }
            else
            {
                GNCLot *doc_lot = sl_is_doc_lot ? scrub_lot : remote_lot;
                GNCLot *pay_lot = sl_is_doc_lot ? remote_lot : scrub_lot;
                Split *ll_doc_split = sl_is_doc_lot ? sl_split : ll_txn_split;
                Split *ll_pay_split = sl_is_doc_lot ? ll_txn_split : sl_split;
                // Ok, let's try to move a payment from pay_lot to doc_lot
                restart_needed = scrub_other_link (pay_lot, ll_pay_split, doc_lot, ll_doc_split);
            }

            // If we got here, the splits in our lot and ll_txn have been severely mixed up
            // And our iterator lists are probably no longer valid
            // So let's start over
            if (restart_needed)
            {
                modified = TRUE;
                goto scrub_start;
            }

        }
    }

    return modified;
}

// Note this is a recursive function. It presumes the number of splits
// in avail_splits is relatively low. With many splits the performance will
// quickly degrade.
// Careful: this function assumes all splits in avail_splits to be valid
// and with values of opposite sign of target_value
// Ignoring this can cause unexpected results!
static SplitList *
gncSLFindOffsSplits (SplitList *avail_splits, gnc_numeric target_value)
{
    gint curr_recurse_level = 0;
    gint max_recurse_level = g_list_length (avail_splits) - 1;

    if (!avail_splits)
        return NULL;

    for (curr_recurse_level = 0;
         curr_recurse_level <= max_recurse_level;
         curr_recurse_level++)
    {
        SplitList *split_iter = NULL;
        for (split_iter = avail_splits; split_iter; split_iter = split_iter->next)
        {
            Split *split = split_iter->data;
            SplitList *match_splits = NULL;
            gnc_numeric split_value, remaining_value;

            split_value = xaccSplitGetValue (split);
            // Attention: target_value and split_value are of opposite sign
            // So to get the remaining target value, they should be *added*
            remaining_value = gnc_numeric_add (target_value, split_value,
                                               GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);

            if (curr_recurse_level == 0)
            {
                if (gnc_numeric_zero_p (remaining_value))
                    match_splits = g_list_prepend (NULL, split);
            }
            else
            {
                if (gnc_numeric_positive_p (target_value) ==
                    gnc_numeric_positive_p (remaining_value))
                    match_splits = gncSLFindOffsSplits (split_iter->next,
                                                        remaining_value);
            }

            if (match_splits)
                return g_list_prepend (match_splits, split);
        }
    }

    return NULL;
}


static gboolean
gncScrubLotDanglingPayments (GNCLot *lot)
{
    SplitList * split_list, *filtered_list = NULL, *match_list = NULL, *node;
    Split *ll_split = gnc_lot_get_earliest_split (lot);
    Transaction *ll_trans = xaccSplitGetParent (ll_split);
    gnc_numeric ll_val = xaccSplitGetValue (ll_split);
    time64 ll_date = xaccTransGetDate (ll_trans);
    const char *ll_desc = xaccTransGetDescription (ll_trans);

    // look for free splits (i.e. not in any lot) which,
    // compared to the lot link split
    // - have the same date
    // - have the same description
    // - have an opposite sign amount
    // - free split's abs value is less than or equal to ll split's abs value
    split_list = xaccAccountGetSplitList(gnc_lot_get_account (lot));
    for (node = split_list; node; node = node->next)
    {
        Split *free_split = node->data;
        Transaction *free_trans;
        gnc_numeric free_val;

        if (NULL != xaccSplitGetLot(free_split))
            continue;

        free_trans = xaccSplitGetParent (free_split);
        if (ll_date != xaccTransGetDate (free_trans))
            continue;

        if (0 != g_strcmp0 (ll_desc, xaccTransGetDescription (free_trans)))
            continue;

        free_val = xaccSplitGetValue (free_split);
        if (gnc_numeric_positive_p (ll_val) ==
            gnc_numeric_positive_p (free_val))
            continue;

        if (gnc_numeric_compare (gnc_numeric_abs (free_val), gnc_numeric_abs (ll_val)) > 0)
            continue;

        filtered_list = g_list_append(filtered_list, free_split);
    }

    match_list = gncSLFindOffsSplits (filtered_list, ll_val);
    g_list_free (filtered_list);

    for (node = match_list; node; node = node->next)
    {
        Split *match_split = node->data;
        gnc_lot_add_split (lot, match_split);
    }

    if (match_list)
    {
        g_list_free (match_list);
        return TRUE;
    }
    else
        return FALSE;
}

static gboolean
gncScrubLotIsSingleLotLinkSplit (GNCLot *lot)
{
    Split *split = NULL;
    Transaction *trans = NULL;

    // Lots with a single split which is also a lot link transaction split
    // may be sign of a dangling payment. Let's try to fix that

    // Only works for single split lots...
    if (1 != gnc_lot_count_splits (lot))
        return FALSE;

    split = gnc_lot_get_earliest_split (lot);
    trans = xaccSplitGetParent (split);

    if (!trans)
    {
        // Ooops - the split doesn't belong to any transaction !
        // This is not expected so issue a warning and continue with next split
        PWARN("Encountered a split in a business lot that's not part of any transaction. "
              "This is unexpected! Skipping split %p.", split);
        return FALSE;
    }

    // Only works if single split belongs to a lot link transaction...
    if (xaccTransGetTxnType (trans) != TXN_TYPE_LINK)
        return FALSE;

    return TRUE;
}

gboolean
gncScrubBusinessLot (GNCLot *lot)
{
    gboolean splits_deleted = FALSE;
    gboolean dangling_payments = FALSE;
    gboolean dangling_lot_link = FALSE;
    Account *acc;
    gchar *lotname=NULL;

    if (!lot) return FALSE;
    lotname = g_strdup (gnc_lot_get_title (lot));
    ENTER ("(lot=%p) %s", lot, lotname ? lotname : "(no lotname)");

    acc = gnc_lot_get_account (lot);
    if (acc)
        xaccAccountBeginEdit(acc);

    // Scrub lot links.
    // They should only remain when two document lots are linked together
    xaccScrubMergeLotSubSplits (lot, FALSE);
    splits_deleted = gncScrubLotLinks (lot);

    // Look for dangling payments and repair if found
    dangling_lot_link = gncScrubLotIsSingleLotLinkSplit (lot);
    if (dangling_lot_link)
    {
        dangling_payments = gncScrubLotDanglingPayments (lot);
        if (dangling_payments)
            splits_deleted |= gncScrubLotLinks (lot);
        else
        {
            Split *split = gnc_lot_get_earliest_split (lot);
            Transaction *trans = xaccSplitGetParent (split);
            xaccTransDestroy (trans);
        }
    }

    // If lot is empty now, delete it
    if (0 == gnc_lot_count_splits (lot))
    {
        PINFO("All splits were removed from lot, deleting");
        gnc_lot_destroy (lot);
    }

    if (acc)
        xaccAccountCommitEdit(acc);

    LEAVE ("(lot=%s, deleted=%d, dangling lot link=%d, dangling_payments=%d)",
            lotname ? lotname : "(no lotname)", splits_deleted, dangling_lot_link,
            dangling_payments);
    g_free (lotname);

    return splits_deleted;
}

/* ============================================================== */

void
gncScrubBusinessAccountLots (Account *acc)
{
    LotList *lots, *node;
    gint lot_count = 0;
    gint curr_lot_no = 1;
    const gchar *str;

    if (!acc) return;
    if (FALSE == xaccAccountIsAPARType (xaccAccountGetType (acc))) return;

    str = xaccAccountGetName(acc);
    str = str ? str : "(null)";

    ENTER ("(acc=%s)", str);
    PINFO ("Cleaning up superfluous lot links in account %s \n", str);
    xaccAccountBeginEdit(acc);

    lots = xaccAccountGetLotList(acc);
    lot_count = g_list_length (lots);
    for (node = lots; node; node = node->next)
    {
        GNCLot *lot = node->data;

        PINFO("Start processing lot %d of %d",
              curr_lot_no, lot_count);

        if (lot)
            gncScrubBusinessLot (lot);

        PINFO("Finished processing lot %d of %d",
              curr_lot_no, lot_count);
        curr_lot_no++;
    }
    g_list_free(lots);
    xaccAccountCommitEdit(acc);
    LEAVE ("(acc=%s)", str);
}

/* ============================================================== */

static void
lot_scrub_cb (Account *acc, gpointer data)
{
    if (FALSE == xaccAccountIsAPARType (xaccAccountGetType (acc))) return;
    gncScrubBusinessAccountLots (acc);
}

void
gncScrubBusinessAccountTreeLots (Account *acc)
{
    if (!acc) return;

    gnc_account_foreach_descendant(acc, lot_scrub_cb, NULL);
    gncScrubBusinessAccountLots (acc);
}

/* ========================== END OF FILE  ========================= */
