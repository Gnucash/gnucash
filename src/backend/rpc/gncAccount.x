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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
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
