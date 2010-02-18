/********************************************************************\
 * policy.h -- Implement Accounting Policy                          *
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
/** @addtogroup Engine
    @{ */
/** @addtogroup Policy Accounting Policy (FIFO/LIFO)
 *  This file implements Accounting Policy.  The Accounting Policy
 *  determines how Splits are assigned to Lots.  The contents
 *  of a Lot determines the Gains on that Lot.  The default policy
 *  is the FIFO policy: the first thing bought is also the first
 *  thing sold.
 @{ */

/** @file policy.h
 *  @brief Implement Accounting Policy.
 *  @author Created by Linas Vepstas August 2003
 *  @author Copyright (c) 2003,2004 Linas Vepstas <linas@linas.org>
 */

#ifndef XACC_POLICY_H
#define XACC_POLICY_H

typedef struct gncpolicy_s GNCPolicy;

/** First-in, First-out Policy
 *  This policy will create FIFO Lots.  FIFO Lots have the following
 *  properties:
 *  -- The lot is started with the earliest posted split that isn't
 *     a part of another lot already.
 *  -- Splits are added to the lot in date order, with earliest splits
 *     added first.
 *  -- All splits in the lot share the same transaction currency as
 *     the split that opened the lot.
 */
GNCPolicy *xaccGetFIFOPolicy (void);

/** Last-in, First-out Policy
 *  This policy will create LIFO Lots.  LIFO Lots have the following
 *  properties:
 *  -- XXX  I think the implementation is broken right now.
 *  -- All splits in the lot share the same transaction currency as
 *     the split that opened the lot.
 */
GNCPolicy *xaccGetLIFOPolicy (void);

#endif /* XACC_POLICY_H */
/** @} */
/** @} */
