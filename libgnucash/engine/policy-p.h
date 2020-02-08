/********************************************************************\
 * policy-p.h -- Implement Accounting Policy Private Header File    *
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

/** @file policy-p.h
 *  @brief Implement Accounting Policy Private header File.
 *  @author Created by Linas Vepstas August 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 *  This file implements Accounting Policy.  The Accounting Policy
 *  determines how splits are assigned to lots.  The default policy
 *  is the FIFO policy: the first thing bought is also the first
 *  thing sold.
 */

#ifndef XACC_POLICY_P_H
#define XACC_POLICY_P_H

#include "gnc-engine.h"
#include "policy.h"

/* ============================================================== */
/** The Policy routines try to encapsulate the FIFO/LIFO-specific
 *  parts of the cap-gains routine, and can be replaced by something
 *  else for other policies.
 *
 *  The PolicyGetLot() routine returns a lot into which the
 *     indicated split should be placed.
 *
 *  The PolicyGetSplit() routine returns an unassinged split
 *     from the account that is appropriate for placing into the
 *     indicated lot.  For the FIFO policy, that would be the
 *     earliest split that is not in any account, and is of the
 *     appropriate sign.  For a LIFO, it would be the latest.
 *
 *  The PolicyIsOpeningSplit() predicate returns a true/false
 *     value, indicating if the indicated split was used to 'open'
 *     or 'grow' the lot.
 *
 *  The PolicyGetLotOpening() routine returns information about
 *     the opening balances for the lot.  The 'opening balances'
 *     are the sum of all the splits used to grow (increase the size
 *     of) the lot.  For a LIFO or FIFO policy, there is only one
 *     split that opens a lot.
 */

struct gncpolicy_s
{
    GNCLot * (*PolicyGetLot) (GNCPolicy *, Split *split);
    Split  * (*PolicyGetSplit) (GNCPolicy *, GNCLot *lot);
    void     (*PolicyGetLotOpening) (GNCPolicy *, GNCLot *lot,
                                     gnc_numeric *ret_amount,
                                     gnc_numeric *ret_value,
                                     gnc_commodity **ret_currency);

    gboolean (*PolicyIsOpeningSplit) (GNCPolicy *, GNCLot *lot,
                                      Split *split);
};

#endif /* XACC_POLICY_P_H */
