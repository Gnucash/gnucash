/*
 * FILE:
 * gncSplit.x
 *
 * FUNCTION:
 * The RPC protocol definition for Gnucash Splits
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#ifndef __GNC_SPLIT_X
#define __GNC_SPLIT_X

#include "gncGUID.x"
#include "gncKVP.x"

struct gncSplit {
  gncGUID	guid;

  /* Different from Gnucash Split */

  gncGUID	acct_guid;	/* Account GUID for this split */
  gncGUID	txn_guid;	/* Parent transaction GUID */

  string	memo<>;
  string	action<>;
  gnc_kvp_frame *	kvp_data;
  char		reconciled;
  gncTimespec	date_reconciled;
  gncNumeric	value;
  gncNumeric	damount;
};

struct gnc_splitlist {
  gncSplit *		split;
  gnc_splitlist *	next;
};

#endif /* __GNC_SPLIT_X */
