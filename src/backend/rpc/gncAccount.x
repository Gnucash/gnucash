/*
 * FILE:
 * gncAccount.x
 *
 * FUNCTION:
 * The RPC definition for a Gnucash Account
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#ifndef __GNC_ACCOUNT_X
#define __GNC_ACCOUNT_X

#include "gncKVP.x"
#include "gncGUID.x"
#include "gncCommodity.x"

#ifdef RPC_HDR
%#include "Account.h"
#endif

struct gncAccount {
  gncGUID	guid;
  string	name<>;
  string	code<>;
  string	desc<>;
  gnc_kvp_frame *	kvp_data;
  enum_t	type;		/* GNCAccountType */

  /* This really sucks -- why do I need to send both a currency and a
   * security?  Well, the engine seems to want it that way.  Ouch.
   */

#ifdef GNCACCT_COMMODITY
  gncCommodityPtr	commodity;
#else
  gncCommodityPtr	currency;
  gncCommodityPtr	security;
#endif

  gncGUID *		parent;
  int		vers;

  gncNumeric	balance;
  gncNumeric	cleared_balance;
  gncNumeric	reconciled_balance;
  gncNumeric	share_balance;
  gncNumeric	share_cleared_balance;
  gncNumeric	share_reconciled_balance;

  bool		core_dirty;
  bool		do_free;
};

struct gnc_acctlist {
  gncAccount *		acct;
  gnc_acctlist *	next;
};

#endif /* __GNC_ACCOUNT_X */
