/*
 * FILE:
 * RpcUtils.h
 *
 * FUNCTION:
 * Implements some utility functions for the RPC (client) backend.
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

#ifndef RPC_UTILS_H
#define RPC_UTILS_H

#include "gncRpc.h"
#include "Account.h"
#include "RpcBackend.h"

/*
 * This number should be increased every time the RPC protocol
 * changes.  This means every time any of the .x file change, this
 * number should be increased.
 */
#define GNCRPC_PROTOCOL_VERSION 1

gnc_kvp_frame *rpcend_build_gnckvp (kvp_frame *frame);
void rpcend_free_gnckvp (gnc_kvp_frame *frame);
kvp_frame * rpcend_parse_gnckvp (gnc_kvp_frame *data);

void rpcend_build_gnctxn (gncTransaction *gnctxn, Transaction *txn);
void rpcend_free_gnctxn (gncTransaction *gnctxn, gboolean freetxn);

void rpcend_build_gncacct (gncAccount *gncacct, Account *acc);
void rpcend_free_gncacct (gncAccount *acc, gboolean freeacct);

gnc_txnlist * rpcend_build_gnctxnlist_list (AccountGroup *ag,
					    gnc_vers_list *txnlist);
void rpcend_free_gnctxnlist (gnc_txnlist *txnlist);

gnc_acctlist * rpcend_build_gncacctlist_list (AccountGroup *ag,
					      gnc_vers_list *acctlist);
gnc_acctlist * rpcend_build_gncacctlist (AccountGroup *ag);
void rpcend_free_gncacctlist (gnc_acctlist *acctlist);

gnc_vers_list * rpcend_build_gncacct_verslist (AccountGroup *ag,
					       gboolean copyguid);
gnc_vers_list * rpcend_build_gnctxn_verslist (AccountGroup *ag,
					      gboolean copyguid);
gnc_vers_list * rpcend_build_gncverslist_txn (GList *txnlist,
					      gboolean copyguid);
void rpcend_free_verslist (gnc_vers_list *vlist, gboolean freeguid);

gnc_commoditylist * rpcend_build_gnccommoditylist (gnc_commodity_table *ct,
						   gboolean copycom);
void rpcend_free_gnccommoditylist (gnc_commoditylist *clist, gboolean freecom);

void rpcend_load_gnccommodity (gnc_commodity_table *ct, gncCommodity *com);
void rpcend_load_commoditylist (gnc_commodity_table *ct,
				gnc_commoditylist *clist);

int rpcend_do_add_acct (AccountGroup *ag, gncAccount * acct,
			gnc_commodity_table *ct);
int rpcend_do_add_txn (gncTransaction * txn, gnc_commodity_table *ct);

void rpcend_build_gncquery (gncQuery *gq, Query *q);
void rpcend_parse_gncquery (gncQuery *gq, Query *q);
void rpcend_free_gncquery (gncQuery *gq);
void rpcend_free_query (Query *q);

#endif /* __RPC_UTILS_H */
