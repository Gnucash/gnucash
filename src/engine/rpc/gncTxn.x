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
