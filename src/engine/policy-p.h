/********************************************************************\
 * policy-.h -- Implement Accounting Policy Private Header File     *
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

/** @file policy-p.h
 *  @breif Implement Accounting Policy Private header File.
 *  @author Created by Linas Vepstas August 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 *  This file implements Accounting Policy.  The Accounting Polciy 
 *  determines how splits are assigned to lots.  The default policy
 *  is teh FIFO policy: the first thing bought is also the first 
 *  thing sold. 
 */

#ifndef XACC_POLICY_P_H 
#define XACC_POLICY_P_H 

#include "gnc-engine.h"

/* ============================================================== */
/** The FIFOPolicy routines try to encapsulate the FIFO-specific
 *  parts of the cap-gains routine, and can eb replaced by something
 *  else for other policies (e.g. LIFO)
 * 
 *  The FIFOPolicyGetLot() routine returns a lot into which the 
 *     indicated split should be placed.
 *
 *  The FIFOPolicyGetSplit() routine returns an unassinged split
 *     from the account that is appropriate for placing into the
 *     indicated lot.  For the FIFO policy, that would be the 
 *     earliest split that is not in any account, and is of the
 *     appropriate sign.
 *
 *  The FIFOPolicyIsOpeningSplit() predicate returns a true/false
 *     value, indicating if the indicated split was used to 'open'
 *     or 'grow' the lot. 
 *
 *  The FIFOPolicyGetLotOpening() routine returns information about
 *     the opening balances for the lot.  The 'opening balances' 
 *     are the sum of all the splits used to grow (increase the size
 *     of) the lot.  For a FIFO polciy, there is only one split 
 *     that opens a lot.
 */

GNCLot * FIFOPolicyGetLot (Split *split, gpointer user_data);

Split * FIFOPolicyGetSplit (GNCLot *lot, gpointer user_data);

void FIFOPolicyGetLotOpening (GNCLot *lot,
        gnc_numeric *ret_amount, gnc_numeric *ret_value,
        gnc_commodity **ret_currency,
        gpointer user_data);

gboolean FIFOPolicyIsOpeningSplit (GNCLot *lot, Split *split, 
                                   gpointer user_data);

#endif /* XACC_POLICY_P_H */
