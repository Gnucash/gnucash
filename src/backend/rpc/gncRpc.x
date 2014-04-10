/*
 * FILE:
 * gncRpc.x
 *
 * FUNCTION:
 * The RPC protocol definition for Gnucash
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

#ifdef RPC_XDR
%#ifndef xdr_enum_t
%#define xdr_enum_t xdr_enum
%#endif
#endif

#include "gncQuery.x"
#include "gncAccount.x"
#include "gncTxn.x"

typedef opaque gncrpc_ptr[8];

/***************************************************************/

struct gncrpc_book_begin_args {
  gncrpc_ptr	book;
  gncrpc_ptr	backend;
  string	book_id<>;
  bool		ignore_lock;
  bool		create;
};

struct gncrpc_book_load_ret {
  int		error;
  gnc_commoditylist *	commodities;	/* List of commodities */
  gnc_acctlist *	acctlist;	/* Full Account tree */
};

struct gncrpc_backend_guid {
  gncrpc_ptr	backend;
  gncGUID	guid;
};

struct gncrpc_backend_txn {
  gncrpc_ptr		backend;
  gncTransaction 	txn;
};

struct gncrpc_commit_acct_args {
  gncrpc_ptr	backend;
  gncAccount	acct;
#ifdef GNCACCT_COMMODITY
  gncCommodity * commodity;
#else
  gncCommodity * currency;
  gncCommodity * security;
#endif
};

struct gncrpc_commit_txn_args {
  gncrpc_ptr	backend;
  gncTransaction 	new;
};

struct gncrpc_sync1_args {
  gncrpc_ptr		backend;
  gnc_commoditylist *	commodities; /* all commodities */
  gnc_vers_list *	acctlist; /* list of all account guid+versions */
  gnc_vers_list *	txnlist; /* list of all txn guid+versions */
};

struct gncrpc_sync1_ret {
  int			error;
  gnc_vers_list *	send_acctlist; /* accts to send to server */
  gnc_vers_list *	send_txnlist; /* txns to send to server */
  gnc_acctlist *	acctlist; /* new or changed accounts from server */
  gnc_txnlist *		txnlist; /* new or changed txns from server */
  gnc_commoditylist *	commodities; /* new commodities from server */
};

struct gncrpc_sync2_args {
  gncrpc_ptr		backend;
  gnc_acctlist *	acctlist; /* new or changed accounts in client */
  gnc_txnlist *		txnlist; /* new or changed txns in client */
};

struct gncrpc_query_args {
  gncrpc_ptr	backend;
  gncGUID *	group_parent_guid;
  gncQuery *	query;
};

struct gncrpc_query_ret {
  int			error;
  gnc_vers_list *	txnlist;
};

struct gncrpc_get_txns_args {
  gncrpc_ptr		backend;
  gnc_vers_list *	guids;
};

struct gncrpc_get_txns_ret {
  int		error;
  gnc_txnlist *	txnlist;
};

/***************************************************************/

program GNCRPC_PROG {
  version GNCRPC_VERS {
    /*
     * int gncrpc_version ()
     *   send the min/max version and get back a suggested version number
     */
    int gncrpc_version () = 0;

    /* Backend functions */
    int gncrpc_book_begin (gncrpc_book_begin_args args) = 1;
    gncrpc_book_load_ret gncrpc_book_load (gncrpc_ptr backend) = 2;
    int gncrpc_book_end (gncrpc_ptr backend) = 3;

    int gncrpc_account_begin_edit (gncrpc_backend_guid) = 4;
    int gncrpc_account_commit_edit (gncrpc_commit_acct_args) = 5;
    int gncrpc_account_rollback_edit (gncrpc_backend_guid) = 6;
    int gncrpc_txn_begin_edit (gncrpc_backend_guid) = 7;
    int gncrpc_txn_commit_edit (gncrpc_commit_txn_args) = 8;
    int gncrpc_txn_rollback_edit (gncrpc_backend_guid) = 9;

    gncrpc_query_ret gncrpc_run_query (gncrpc_query_args) = 10;
    gncrpc_sync1_ret gncrpc_sync1 (gncrpc_sync1_args) = 11;
    int gncrpc_sync2 (gncrpc_sync2_args) = 12;

    /* Helper functions */
    gncrpc_get_txns_ret gncrpc_get_txns (gncrpc_get_txns_args) = 13;

  } = 1;
} = 729284;
