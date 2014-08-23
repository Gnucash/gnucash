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

static QofLogModule log_module = GNC_MOD_LOT;

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
    gnc_numeric new_val, rem_val;
    Split *to_reduce, *rem_split;
    Transaction *txn;
    GNCLot *lot;

    if (gnc_numeric_positive_p (valA) == gnc_numeric_positive_p (valB))
        return FALSE; //Splits are not of opposite sign

    if (gnc_numeric_equal (gnc_numeric_abs (valA), gnc_numeric_abs (valB)))
        return FALSE; // Splits have an equal value (but with opposite sign - nothing to do
    else if (gnc_numeric_compare (gnc_numeric_abs (valA), gnc_numeric_abs (valB)) > 0)
    {
        to_reduce = splitA;
        new_val = gnc_numeric_neg (valB);
    }
    else
    {
        to_reduce = splitB;
        new_val = gnc_numeric_neg (valA);
    }

    rem_val = gnc_numeric_add (valA, valB, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD); // note: values are of opposite sign
    rem_split = xaccMallocSplit (xaccSplitGetBook (to_reduce));
    xaccSplitCopyOnto (to_reduce, rem_split);
    xaccSplitSetValue (rem_split, rem_val);

    txn = xaccSplitGetParent (to_reduce);
    xaccTransBeginEdit (txn);
    xaccSplitSetValue (to_reduce, new_val);
    xaccSplitSetParent (rem_split, txn);
    xaccTransCommitEdit (txn);

    lot = xaccSplitGetLot (to_reduce);
    gnc_lot_add_split (lot, rem_split);
    return TRUE;
}

static Split *get_pay_split (GNCLot *pay_lot, Split *ll_split)
{
    SplitList *pls_iter = NULL;

    if (!pay_lot)
        return NULL;

    for (pls_iter = gnc_lot_get_split_list (pay_lot); pls_iter; pls_iter = pls_iter->next)
    {
        Split *pay_split = pls_iter->data;
        Transaction *pay_txn;
        gnc_numeric ll_val, pay_val;

        if (!pay_split)
            continue;


        pay_txn = xaccSplitGetParent (pay_split);
        if (!pay_txn)
        {
            // Ooops - the split doesn't belong to any transaction !
            // This is not expected so issue a warning and continue with next split
            PWARN("Encountered a split in a payment lot that's not part of any transaction. "
                  "This is unexpected! Skipping split %p.", pay_split);
            continue;
        }

        // We're only interested in the non-lot link txn splits
        if (xaccTransGetTxnType (pay_txn) == TXN_TYPE_LINK)
            continue;

        // Check if this split has the opposite sign of the lot link split we want to scrub
        ll_val = xaccSplitGetValue (ll_split);
        pay_val = xaccSplitGetValue (pay_split);
        if (gnc_numeric_positive_p (ll_val) == gnc_numeric_positive_p (pay_val))
            continue;

        // Bingo - if we get to this point, we have found a split that
        // - does belong to a transaction
        // - has the opposite sign of the lot link split we're scrubbing
        // - is not a lot link split in itself
        return pay_split;

    }

    // Not valid split found
    return NULL;
}

static void
scrub_doc_doc_link (Transaction *ll_txn)
{
    gchar *new_memo;
    SplitList *lts_iter;
    SplitList *splits = NULL, *siter;
    GList *titles = NULL, *titer;

    // Find all splits in the lot link transaction that are also in a document lot
    for (lts_iter = xaccTransGetSplitList (ll_txn); lts_iter; lts_iter = lts_iter->next)
    {
        Split *split = lts_iter->data;
        GNCLot *lot;
        GncInvoice *invoice;
        gchar *title;

        if (!split)
            continue;

        lot = xaccSplitGetLot (split);
        if (!lot)
            continue;

        invoice = gncInvoiceGetInvoiceFromLot (lot);
        if (!invoice)
            continue;

        title = g_strdup_printf ("%s %s", gncInvoiceGetTypeString (invoice), gncInvoiceGetID (invoice));

        titles = g_list_insert_sorted (titles, title, (GCompareFunc)g_strcmp0);
        splits = g_list_prepend (splits, split); // splits don't need to be sorted
    }

    if (!titles)
        return; // We didn't find document lots

    // Create the memo as we'd want it to be
    new_memo = g_strconcat (_("Offset between documents: "), titles->data, NULL);
    for (titer = titles->next; titer; titer = titer->next)
    {
        gchar *tmp_memo = g_strconcat (new_memo, " - ", titer->data, NULL);
        g_free (new_memo);
        new_memo = tmp_memo;
    }
    g_list_free_full (titles, g_free);

    // Update the memos of all the splits we found previously (if needed)
    for (siter = splits; siter; siter = siter->next)
    {
        if (g_strcmp0 (xaccSplitGetMemo (siter->data), new_memo) != 0)
            xaccSplitSetMemo (siter->data, new_memo);
    }

    g_list_free (splits);
    g_free (new_memo);
}

static gboolean
scrub_doc_pay_link (GNCLot *doc_lot, Split *ll_doc_split,
                    GNCLot *pay_lot, Split *ll_pay_split)
{
    Split *real_pay_split; // This refers to the split in the payment lot representing the payment itself
    gnc_numeric doc_val, pay_val, real_pay_val;
    gboolean modified = FALSE;
    Transaction *ll_txn = xaccSplitGetParent (ll_doc_split);

    // Per iteration we can only scrub at most max (val-doc-split, val-pay-split)
    // So split the bigger one in two if needed and continue with the equal valued splits only
    // The remainder is added to the lot link transaction and the lot to keep everything balanced
    // and will be processed in a future iteration
    modified = reduce_biggest_split (ll_doc_split, ll_pay_split);

    // Next we have to find the original payment split so we can
    // add (part of) it to the document lot
    real_pay_split = get_pay_split (pay_lot, ll_pay_split);
    if (!real_pay_split)
        return modified; // No usable split in the payment lot

    // Here again per iteration we can only scrub at most max (val-other-pay-split, val-pay-split)
    // So split the bigger one in two if needed and continue with the equal valued splits only
    // The remainder is added to the lot link transaction and the lot to keep everything balanced
    // and will be processed in a future iteration
    modified = reduce_biggest_split (real_pay_split, ll_pay_split);

    // Once more check for max (val-doc-split, val-pay-split), and reduce if necessary.
    // It may have changed while looking for the real payment split
    modified = reduce_biggest_split (ll_doc_split, ll_pay_split);

    // At this point ll_doc_split and real_pay_split should have the same value
    // If not, flag a warning and skip to the next iteration
    doc_val = xaccSplitGetValue (ll_doc_split);
    pay_val = xaccSplitGetValue (ll_pay_split);
    real_pay_val = xaccSplitGetValue (real_pay_split);
    if (!gnc_numeric_equal (doc_val, real_pay_val))
    {
        // This is unexpected - write a warning message and skip this split
        PWARN("real_pay_val and doc_val differ. "
              "This is unexpected! Skip scrubbing of real_pay_split %p against ll_doc_split %p.", real_pay_split, ll_doc_split);
        return modified;
    }

    // Now do the actual split dance
    // - move real payment split to doc lot
    // - delete both lot link splits from the lot link transaction
    gnc_lot_add_split (doc_lot, real_pay_split);
    xaccTransBeginEdit (ll_txn);
    xaccSplitDestroy (ll_doc_split);
    xaccSplitDestroy (ll_pay_split);
    xaccTransCommitEdit (ll_txn);

    // Cleanup the lots
    xaccScrubMergeLotSubSplits (doc_lot, FALSE);
    xaccScrubMergeLotSubSplits (pay_lot, FALSE);

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

            // Only splits of opposite sign can be scrubbed
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
            //   This situation will be skipped as it's not clear what the best solution
            //   will be. In the most common case there's at least one document involved
            //   in the transaction as well and the links will be resolved in a future iteration.
            //   The more uncommon case of canceling a payment with a refund is not handled yet.
            // - A document and a payment lot:
            //   (Part of) the link will be eliminated and instead (part of) the real
            //   payment will be added to the document lot to handle the payment.
            if (sl_is_doc_lot && rl_is_doc_lot)
                scrub_doc_doc_link (ll_txn);
            else if (!sl_is_doc_lot && !rl_is_doc_lot)
                continue; // next lot link transaction split
            else
            {
                // Now determine the document lot/split and the payment lot/split
                GNCLot *doc_lot = sl_is_doc_lot ? scrub_lot : remote_lot;
                GNCLot *pay_lot = sl_is_doc_lot ? remote_lot : scrub_lot;
                Split *ll_doc_split = sl_is_doc_lot ? sl_split : ll_txn_split;
                Split *ll_pay_split = sl_is_doc_lot ? ll_txn_split : sl_split;
                restart_needed = scrub_doc_pay_link ( doc_lot, ll_doc_split, pay_lot, ll_pay_split);
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


gboolean
gncScrubBusinessLot (GNCLot *lot)
{
    gboolean splits_deleted = FALSE;
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

    // If lot is empty now, delete it
    if (0 == gnc_lot_count_splits (lot))
    {
        PINFO("All splits were removed from lot, deleting");
        gnc_lot_destroy (lot);
    }

    if (acc)
        xaccAccountCommitEdit(acc);

    LEAVE ("(lot=%s, deleted=%d)", lotname ? lotname : "(no lotname)", splits_deleted);
    g_free (lotname);

    return splits_deleted;
}

/* ============================================================== */

void
gncScrubBusinessAccountLots (Account *acc)
{
    LotList *lots, *node;
    if (!acc) return;
    if (FALSE == xaccAccountIsAPARType (xaccAccountGetType (acc))) return;

    ENTER ("(acc=%s)", xaccAccountGetName(acc));
    xaccAccountBeginEdit(acc);

    lots = xaccAccountGetLotList(acc);
    for (node = lots; node; node = node->next)
    {
        GNCLot *lot = node->data;
        if (lot)
            gncScrubBusinessLot (lot);
    }
    g_list_free(lots);
    xaccAccountCommitEdit(acc);
    LEAVE ("(acc=%s)", xaccAccountGetName(acc));
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
