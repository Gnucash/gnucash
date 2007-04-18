/********************************************************************\
 * AccountP.h -- Account engine-private data structure              *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2002, Linas Vepstas <linas@linas.org>         *
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
 *                                                                  *
\********************************************************************/

/** @file AccountP.h
 *
 * This is the *private* header for the account structure.
 * No one outside of the engine should ever include this file.
 *
 * This header includes prototypes for "dangerous" functions.
 * Invoking any of these functions potentially leave the account
 * in an inconsistent state.  If they are not used in the proper
 * setting, they can leave the account structures in an inconsistent
 * state.  Thus, these methods should never be used outside of
 * the engine, which is why they are "hidden" here. 
 *
 */

#ifndef XACC_ACCOUNT_P_H
#define XACC_ACCOUNT_P_H

#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"

/** STRUCTS *********************************************************/

/** This is the data that describes an account. 
 *
 * This is the *private* header for the account structure.
 * No one outside of the engine should ever include this file.
*/

/** \struct Account */
struct account_s
{
  QofInstance inst;

  /* 
   * The commodity field denotes the kind of 'stuff' stored 
   * in this account.  The 'amount' field of a split indicates
   * how much of the 'stuff' there is.
   */
  gnc_commodity * commodity;
  int commodity_scu;
  gboolean non_standard_scu;

  /* -------------------------------------------------------------- */
  /* Backend private expansion data */
  guint32  idata;     /* used by the sql backend for kvp management */
};

/* Set the account's GUID. This should only be done when reading
 * an account from a datafile, or some other external source. Never
 * call this on an existing account! */
void xaccAccountSetGUID (Account *account, const GUID *guid);

/* Register Accounts with the engine */
gboolean xaccAccountRegister (void);

/** killed for now, need to resurect this or something similar
 *  * for transactional/dirty kvp.  Later.  Right now a place holder
 *   */
#define xaccAccountSetSlots_nc(A,S) qof_instance_set_slots(QOF_INSTANCE(A),S)

#endif /* XACC_ACCOUNT_P_H */
