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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 */

#ifndef XACC_POLICY_H 
#define XACC_POLICY_H 

typedef struct gncpolicy_s GNCPolicy;

/** First-in, First-out Policy */
GNCPolicy *xaccGetFIFOPolicy (void); 

/** Last-in, First-out Policy */
GNCPolicy *xaccGetLIFOPolicy (void); 

#endif /* XACC_POLICY_H */
/** @} */
/** @} */
