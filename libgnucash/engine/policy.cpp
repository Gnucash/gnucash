/********************************************************************\
 * policy.c -- Implement FIFO Accounting Policy                     *
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

/** @file policy.c
 *  @brief Implement FIFO Accounting Policy.
 *  @author Created by Linas Vepstas August 2003
 *  @author Copyright (c) 2003,2004 Linas Vepstas <linas@linas.org>
 *
 *  This file implements the FIFO Accounting Policy (and, in the
 *  future, others as well).  The Accounting Polciy determines
 *  how splits are assigned to lots.
 */

#include <config.h>

#include <glib.h>

#include "Account.h"
#include "Account.hpp"
#include "Transaction.h"
#include "TransactionP.hpp"
#include "cap-gains.h"
#include "gnc-engine.h"
#include "gnc-lot.h"
#include "policy.h"
#include "policy-p.h"

#ifndef SWIG             /* swig doesn't see N_() as a string constant */
#include <glib/gi18n.h>
#else
#define N_(string) string
#endif

#define FIFO_POLICY            "fifo"
#define FIFO_POLICY_DESC    N_("First In, First Out")
#define FIFO_POLICY_HINT    N_("Use oldest lots first.")
#define LIFO_POLICY            "lifo"
#define LIFO_POLICY_DESC    N_("Last In, First Out")
#define LIFO_POLICY_HINT    N_("Use newest lots first.")
#define AVERAGE_POLICY         "average"
#define AVERAGE_POLICY_DESC N_("Average")
#define AVERAGE_POLICY_HINT N_("Average cost of open lots.")
#define MANUAL_POLICY          "manual"
#define MANUAL_POLICY_DESC  N_("Manual")
#define MANUAL_POLICY_HINT  N_("Manually select lots.")

//static QofLogModule log_module = GNC_MOD_LOT;

static Split *
DirectionPolicyGetSplit (GNCPolicy *pcy, GNCLot *lot, short reverse)
{
    Split *split;
    gnc_commodity *common_currency;
    gboolean want_positive;
    gnc_numeric baln;
    Split *osplit;
    Transaction *otrans;
    time64 open_time;
    Account* lot_account;

    if (!pcy || !lot || !gnc_lot_get_split_list(lot)) return nullptr;
    lot_account = gnc_lot_get_account(lot);
    if (!lot_account) return nullptr;

    /* Recomputing the balance re-evaluates the lot closure */
    baln = gnc_lot_get_balance (lot);
    if (gnc_lot_is_closed(lot)) return nullptr;

    want_positive = gnc_numeric_negative_p (baln);

    /* All splits in lot must share a common transaction currency. */
    split = GNC_SPLIT(gnc_lot_get_split_list(lot)->data);
    common_currency = split->parent->common_currency;

    /* Don't add a split to the lot unless it will be the new last
       split in the lot.  Otherwise our balance tests will be wrong
       and the lot may end up too thin or too fat. */
    osplit = gnc_lot_get_latest_split (lot);
    otrans = osplit ? xaccSplitGetParent (osplit) : 0;
    open_time = xaccTransRetDatePosted (otrans);

    /* Walk over *all* splits in the account, till we find one that
     * hasn't been assigned to a lot.  Return that split.
     * Make use of the fact that the splits in an account are
     * already in date order; so we don't have to sort. */
    auto find_split = [open_time, common_currency, want_positive](const Split* split)
    {
        return (!split->lot &&
                xaccTransRetDatePosted (xaccSplitGetParent (split)) >= open_time &&
                gnc_commodity_equiv (common_currency, split->parent->common_currency) &&
                !gnc_numeric_zero_p (split->amount) &&
                want_positive == gnc_numeric_positive_p (split->amount));
    };

    return gnc_account_find_split (lot_account, find_split, reverse);
}

/* ============================================================== */

static GNCLot *
FIFOPolicyGetLot (GNCPolicy *pcy, Split *split)
{
    if (!split) return nullptr;
    return xaccAccountFindEarliestOpenLot (split->acc, split->amount,
                                           split->parent->common_currency);
}

static Split *
FIFOPolicyGetSplit (GNCPolicy *pcy, GNCLot *lot)
{
    return DirectionPolicyGetSplit (pcy, lot, 0);
}

static void
FIFOPolicyGetLotOpening (GNCPolicy *pcy,
                         GNCLot *lot,
                         gnc_numeric *ret_amount, gnc_numeric *ret_value,
                         gnc_commodity **ret_currency)
{
    Split *opening_split;
    opening_split = gnc_lot_get_earliest_split(lot);

    if (ret_amount) *ret_amount = opening_split->amount;
    if (ret_value) *ret_value = opening_split->value;
    if (ret_currency) *ret_currency = opening_split->parent->common_currency;
}

static gboolean
FIFOPolicyIsOpeningSplit (GNCPolicy *pcy, GNCLot *lot, Split *split)
{
    Split *opening_split;
    opening_split = gnc_lot_get_earliest_split(lot);
    return (split == opening_split);
}

GNCPolicy *
xaccGetFIFOPolicy (void)
{
    static GNCPolicy *pcy = nullptr;

    if (!pcy)
    {
        pcy = g_new (GNCPolicy, 1);
        pcy->PolicyGetLot = FIFOPolicyGetLot;
        pcy->PolicyGetSplit = FIFOPolicyGetSplit;
        pcy->PolicyGetLotOpening = FIFOPolicyGetLotOpening;
        pcy->PolicyIsOpeningSplit = FIFOPolicyIsOpeningSplit;
    }
    return pcy;
}


/* =========================== END OF FILE ======================= */
