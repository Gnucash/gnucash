/*
 * FILE:
 * gncTxn.x
 *
 * FUNCTION:
 * The RPC protocol definition for Gnucash Transactions
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef __GNC_TXN_X
#define __GNC_TXN_X

#include "gncCommodity.x"
#include "gncGUID.x"
#include "gncKVP.x"
#include "gncSplit.x"

struct gncTransaction {
  gncGUID		guid;
  gncTimespec		date_entered;
  gncTimespec		date_posted;
  string		num<>;
  string		desc<>;
  gnc_kvp_frame*	kvp_data;
  gncCommodityPtr	common_currency;
  int			vers;
  /* Different from here */
  gnc_splitlist *	splits;
  bool			do_free;
};

struct gnc_txnlist {
  gncTransaction *	txn;
  gnc_txnlist *		next;
};

#endif /* __GNC_TXN_X */
