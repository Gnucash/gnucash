/*
 * FILE:
 * RpcBackend.c
 *
 * FUNCTION:
 * Implements the callbacks for the RPC (client) backend.
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#define _GNU_SOURCE

#include "config.h"

#include <rpc/xprt_thrd.h>

#include <glib.h>
#include <stdio.h>  
#include <string.h>  
#include <sys/types.h>  
#include <unistd.h>  
#include <assert.h>
#include <pthread.h>

#include "AccountP.h"
#include "Backend.h"
#include "Group.h"
#include "GroupP.h"
#include "gnc-book.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "guid.h"
#include "TransactionP.h"

#include "gncRpc.h"
#include "RpcBackend.h"
#include "RpcSock.h"
#include "RpcUtils.h"

#define RPCEND_MAGIC	0x49c3a61c
#define VERIFY_BE(be,ret) { \
	if ((be) == NULL) \
		return (ret); \
	assert (be->magic == RPCEND_MAGIC); \
	}
#define VERIFY_BEV(be) { \
	if ((be) == NULL) \
		return; \
	assert (be->magic == RPCEND_MAGIC); \
	}

struct _rpcend {
  Backend	be;		/* Backend */
  gint32	magic;		/* Magic number */

  char *	hostname;	/* RPC Server */
  char *	portNum;	/* Server Port Number */
  char *	dbName;		/* Foreign Database Name */

  GNCBook *	book;		/* My GNC Book, for insertions */

  TXPRT *	xprt;		/* RPC Transport */
  CLIENT *	client;		/* RPC Client */
  RPCSock *	sock;		/* Socket */

  pthread_mutex_t eventlock;	/* Mutex around events members */
  gboolean	events;		/* Are there events? */

  /* For saving the hooks */
  int		nest_count;	/* counter for nested disables */
  Backend	snr;
};

static void rpcendInit (RPCBackend *);

static short module = MOD_BACKEND;

/*******************************************************************/
/* Internal helper functions                                       */

static void rpcendEnable (RPCBackend *be)
{
   if (be->nest_count <= 0)
     PERR ("too many nested disables");

   be->nest_count --;
   PINFO("nest count=%d", be->nest_count);
   if (be->nest_count) return;

   /* restore hooks */
   be->be.account_begin_edit  = be->snr.account_begin_edit;
   be->be.account_commit_edit = be->snr.account_commit_edit;
   be->be.trans_begin_edit    = be->snr.trans_begin_edit;
   be->be.trans_commit_edit   = be->snr.trans_commit_edit;
   be->be.trans_rollback_edit = be->snr.trans_rollback_edit;
   be->be.run_query           = be->snr.run_query;
   be->be.sync                = be->snr.sync;
}

static void rpcendDisable (RPCBackend *be)
{
   if (be->nest_count < 0)
     PERR ("too many nested enables");

   be->nest_count ++;
   PINFO("nest count=%d", be->nest_count);
   if (be->nest_count > 1) return;

   /* save hooks */
   be->snr.account_begin_edit  = be->be.account_begin_edit;
   be->snr.account_commit_edit = be->be.account_commit_edit;
   be->snr.trans_begin_edit    = be->be.trans_begin_edit;
   be->snr.trans_commit_edit   = be->be.trans_commit_edit;
   be->snr.trans_rollback_edit = be->be.trans_rollback_edit;
   be->snr.run_query           = be->be.run_query;
   be->snr.sync                = be->be.sync;

   /* And turn off future calls */
   be->be.account_begin_edit  = NULL;
   be->be.account_commit_edit = NULL;
   be->be.trans_begin_edit    = NULL;
   be->be.trans_commit_edit   = NULL;
   be->be.trans_rollback_edit = NULL;
   be->be.run_query           = NULL;
   be->be.sync                = NULL;
}

static void myClose (void *arg)
{
  RPCBackend *be = (RPCBackend *)arg;

  if (be->client)
    CLNT_DESTROY(be->client);
  be->client = NULL;

  if (be->xprt)
    TXPRT_DESTROY (be->xprt);
  be->xprt = NULL;

  be->sock = NULL;

  /* Turn off all the callbacks (should we do this manually?) */
  rpcendDisable (be);

  /* Should we set an event, here? */

  /* Don't worry about setting the error here -- it will get set if
   * the backend is called again
   */
}

static void rpcendLogin (RPCBackend *be)
{
  unsigned short portNum = 0;
  RPCSock *sock = NULL;
  TXPRT *xprt = NULL;
  CLIENT *client = NULL;
  int err;

  ENTER ("be=%p, host=\"%s\", port=\"%s\"", be,
         be->hostname ? be->hostname : "",
         be->portNum ? be->portNum : "");

  /* Figure out the port number */
  if (be->portNum)
    portNum = (unsigned short) atoi (be->portNum);

  if (!portNum)
    portNum = RPCEND_PORT;

  /* Convert portnum to net byte order */
  portNum = htons (portNum);

  /* Connect to the RPC Server */
  if ((err = RpcConnect (be->hostname, portNum, &sock)) != 0) {
    xaccBackendSetError (&be->be, err);
    LEAVE ("connect");
    return;
  }

  /* Build the RPC Transport */
  if ((err = RpcTransport (sock, myClose, (void *)be, &xprt)) != 0) {
    RpcClose (sock);
    xaccBackendSetError (&be->be, err);
    LEAVE ("transport");
    return;
  }

  /* Get the RPC Client */
  client = TXPRT_NEW_CLIENT (xprt, GNCRPC_PROG, GNCRPC_VERS);
  if (client == NULL) {
    RpcClose (sock);
    xaccBackendSetError (&be->be, ERR_BACKEND_ALLOC);
    LEAVE ("client");
    return;
  }

  /* Verify the RPC Server Version */
  err = 0;
  gncrpc_version_1 (NULL, &err, client);
  if (err != GNCRPC_PROTOCOL_VERSION) {
    CLNT_DESTROY (client);
    TXPRT_DESTROY (xprt);
    RpcClose (sock);
    xaccBackendSetError (&be->be, ERR_RPC_BAD_VERSION);
    LEAVE ("rpc version");
    return;
  }

  /* "Login" to the server */


  /* Save the rpc information */
  be->xprt = xprt;
  be->sock = sock;
  be->client = client;
  
  LEAVE ("ok");
  return;
}

static void rpcend_add_gncaccount (RPCBackend *be, gncAccount *acct,
				   AccountGroup *topgrp)
{
  gnc_commodity_table *ct = gnc_book_get_commodity_table (be->book);
  rpcend_do_add_acct (topgrp, acct, ct);
  return;
}

static void rpcend_add_gncacctlist (RPCBackend *be,
				      AccountGroup *topgrp,
				      gnc_acctlist *acctlist)
{
  for (; acctlist != NULL; acctlist = acctlist->next) {
    rpcend_add_gncaccount (be, acctlist->acct, topgrp);
  }
}

/* Return an int depicting whether this txn would be added to the engine:
 * 	-1 == transaction is new(er) and would be added
 *	 0 == transaction version numbers match
 *	 1 == transaction is older than current engine cache
 */
static int rpcend_would_add_txn (gnc_vers_list *txn)
{
  Transaction *trans;
  GUID *guid;
  int version;

  if (!txn)
    return 0;

  guid = (GUID *)(txn->guid);
  trans = xaccTransLookup (guid);
  if (!trans)
    return -1;

  version = xaccTransGetVersion (trans);
  if (version == txn->vers)
    return 0;
  else if (version > txn->vers)
    return 1;
  else
    return -1;
}

/* Return an int depicting whether this txn was added to the engine:
 * 	-1 == transaction is new(er) and was added
 *	 0 == transaction version numbers match; no changes
 *	 1 == transaction is older than current engine cache; no changes
 */
static int rpcend_add_transaction (RPCBackend *be, gncTransaction *txn)
{
  int cache_is_newer;
  gnc_commodity_table *ct;

  if (!be || !txn) return 0;

  /* disable callbacks and GUI events */
  gnc_engine_suspend_events ();
  rpcendDisable (be);

  ct = gnc_book_get_commodity_table (be->book);
  cache_is_newer = rpcend_do_add_txn (txn, ct);

  /* re-enable events to the backend and GUI */
  rpcendEnable (be);
  gnc_engine_resume_events ();

  return cache_is_newer;
}

static int rpcend_add_gnctransaction_list (RPCBackend *be, gnc_txnlist *tl)
{
  for (; tl != NULL; tl = tl->next) {
    rpcend_add_transaction (be, tl->txn);
  }
  return 0;
}

static void rpcend_add_gnccommoditylist (RPCBackend *be, gnc_commoditylist *cl)
{
  gnc_commodity_table *ct = gnc_book_get_commodity_table (be->book);

  rpcend_load_commoditylist (ct, cl);
}


/***************************************************************************/
/* Below this line are the 'exported interface' functions                  */

/*
 * book_load will only load the commodity table and account tree 
 */
static AccountGroup * rpcend_book_load (Backend *bend)
{
  RPCBackend *be = (RPCBackend *)bend;
  AccountGroup *ag = NULL;
  gncrpc_ptr backend;
  gncrpc_book_load_ret ret;

  VERIFY_BE (be, NULL);

  ENTER ("be=%p", be);

  /* XXX: Handle the case where the connection was closed! */

  memset (backend, 0, sizeof(backend));
  memset (&ret, 0, sizeof (ret));
  memcpy (backend, (char *)&be, sizeof(be));
  gncrpc_book_load_1 (backend, &ret, be->client);

  if (ret.error != 0) {
    xaccBackendSetError (&be->be, ret.error);
    return NULL;
  }

  /* suspend events */
  gnc_engine_suspend_events ();
  rpcendDisable (be);

  /* Build the Commodity Table */
  rpcend_add_gnccommoditylist (be, ret.commodities);

  /* Parse the AccountGroup */
  ag = gnc_book_get_group (be->book);
  if (!ag)
    ag = xaccMallocAccountGroup ();
  rpcend_add_gncacctlist (be, ag, ret.acctlist);

  /* Mark the newly read group as saved, since the act of putting
   * it together will have caused it to be marked up as not-saved.
   */
  xaccGroupMarkSaved (ag);

  rpcendEnable (be);
  gnc_engine_resume_events ();

  /* Free the RPC results */
  CLNT_FREERES (be->client, (xdrproc_t)xdr_gncrpc_book_load_ret, (caddr_t)&ret);

  LEAVE ("be=%p, ag=%p", be, ag);

  return ag;
}

static void rpcend_book_end (Backend *bend)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_ptr backend;
  int res = 0;
  VERIFY_BEV (be);

  ENTER ("be=%p", be);

  /* XXX: Handle the case where the connection was closed */
  if (be->client) {
    memset (backend, 0, sizeof(backend));
    memcpy (backend, (char *)&be, sizeof(be));
    gncrpc_book_end_1 (backend, &res, be->client);
    if (res != 0)
      xaccBackendSetError (&be->be, res);
  }

  rpcendDisable (be);
  /* Disconnect and cleanup XXX */

  /* Now close the socket */
  if (be->sock)
    RpcClose (be->sock);

  LEAVE ("be=%p", be);
}

static int rpcend_account_begin_edit (Backend *bend, Account *acct)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_backend_guid args;
  int ret = 0;
  VERIFY_BE (be, -1);

  ENTER ("be=%p, acc=%p", be, acct);

  memset (&args, 0, sizeof (args));
  memcpy (args.backend, (char *)&be, sizeof(be));
  memcpy (args.guid, acct->guid.data, sizeof (args.guid));

  gncrpc_account_begin_edit_1 (&args, &ret, be->client);
  LEAVE ("be=%p, acc=%p (%s)", be, acct, acct ? acct->accountName : "");
  return ret;
}

static int rpcend_account_rollback_edit (Backend *bend, Account *acct)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_backend_guid args;
  int ret = 0;
  VERIFY_BE (be, -1);

  ENTER ("be=%p, acc=%p (%s)", be, acct, acct ? acct->accountName : "");

  if (acct == NULL)
    return ERR_BACKEND_MISC;

  memset (&args, 0, sizeof (args));
  memcpy (args.backend, (char *)&be, sizeof(be));
  memcpy (args.guid, acct->guid.data, sizeof (args.guid));

  gncrpc_account_rollback_edit_1 (&args, &ret, be->client);

  LEAVE ("be=%p, acc=%p, ret=%d", be, acct, ret);
  return ret;
}

static int rpcend_account_commit_edit (Backend *bend, Account *acct)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_commit_acct_args args;
  int ret = 0;
  AccountGroup *parent;
  VERIFY_BE (be, -1);

  ENTER ("be=%p, acc=%p (%s)", be, acct, acct ? acct->accountName : "");

  if (acct == NULL)
    return ERR_BACKEND_MISC;

  parent = xaccAccountGetParent(acct);

  /* First, see if we need to do anything here */
  if (acct->core_dirty == FALSE) {
    if (parent) parent->saved = 1;
    PINFO ("core_dirty == FALSE");
    return (rpcend_account_rollback_edit (bend, acct));
  } else {
    acct->version++;
  }

  memset (&args, 0, sizeof (args));
  memcpy (args.backend, (char *)&be, sizeof(be));

  /* Copy the Account information */
  rpcend_build_gncacct (&(args.acct), acct);
#ifdef GNCACCT_COMMODITY
  args.commodity = (gncCommodity *) xaccAccountGetCommodity (acct);
#else
  args.currency = (gncCommodity *) xaccAccountGetCurrency (acct);
  args.security = (gncCommodity *) xaccAccountGetSecurity (acct);
#endif

  gncrpc_account_commit_edit_1 (&args, &ret, be->client);

  /* Free up allocated data (in particular the kvp_frame */
  rpcend_free_gnckvp (args.acct.kvp_data);

  /* Set the save flag in the parent, even if the RPC failed */
  if (parent) parent->saved = 1;

  if (ret != 0) {
    /* Failed to write; revert our copy */
    acct->version--;
  }

  LEAVE ("be=%p, acc=%p, ret=%d", be, acct, ret);
  return ret;
}

static int rpcend_trans_begin_edit (Backend *bend, Transaction *txn)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_backend_guid args;
  int ret = 0;
  VERIFY_BE (be, -1);

  ENTER ("be=%p, txn=%p", be, txn);

  memset (&args, 0, sizeof (args));
  memcpy (args.backend, (char *)&be, sizeof(be));
  memcpy (args.guid, txn->guid.data, sizeof (args.guid));

  gncrpc_txn_begin_edit_1 (&args, &ret, be->client);
  LEAVE ("be=%p, txn=%p", be, txn);
  return ret;
}

static int rpcend_trans_commit_edit (Backend *bend, Transaction *new,
				     Transaction *orig)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_commit_txn_args args;
  int ret = 0;
  VERIFY_BE (be, -1);
  ENTER ("be=%p, new=%p, vers=%d", be, new, new->version);

  new->version++;

  memset (&args, 0, sizeof (args));
  memcpy (args.backend, (char *)&be, sizeof(be));
  rpcend_build_gnctxn (&args.new, new);

  gncrpc_txn_commit_edit_1 (&args, &ret, be->client);

  rpcend_free_gnctxn (&args.new, FALSE);

  if (ret != 0) {
    /* Failed to write; backoff our version number */
    new->version--;
  }
  
  LEAVE ("be=%p, new=%p, ret=%d, txn_vers=%d", be, new, ret, new->version);
  return ret;
}

static int rpcend_trans_rollback_edit (Backend *bend, Transaction *txn)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_backend_guid args;
  int ret = 0;
  VERIFY_BE (be, -1);
  ENTER ("be=%p, txn=%p", be, txn);
  memset (&args, 0, sizeof (args));
  memcpy (args.backend, (char *)&be, sizeof(be));
  memcpy (args.guid, txn->guid.data, sizeof (args.guid));

  gncrpc_txn_rollback_edit_1 (&args, &ret, be->client);

  LEAVE ("be=%p, txn=%p, ret=%d", be, txn, ret);
  return ret;
}

static void rpcend_run_query (Backend *bend, Query *q)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_query_args args;
  gncrpc_query_ret ret;
  gncrpc_get_txns_args txns_args;
  gncrpc_get_txns_ret txns_ret;

  VERIFY_BEV (be);
  if (!q) return;

  ENTER ("be=%p, q=%p", be, q);
  memset (&args, 0, sizeof (args));
  memset (&ret, 0, sizeof (ret));
  memcpy (args.backend, (char *)&be, sizeof(be));

  /*  xaccQueryPrint (q);*/

  /* Save off some non-transmitted information from the Query, and
   * restore it after we run the query
   */
  {
    AccountGroup *actg;
    gncQuery gq;

    rpcend_build_gncquery (&gq, q);
    args.query = &gq;

    actg = (AccountGroup *)gq.acct_group;
    gq.acct_group = NULL;
    gq.split_list = NULL;
    gq.xtn_list = NULL;

    if (actg && actg->parent)
      args.group_parent_guid = (gncGUID *) &(actg->parent->guid);

    PINFO ("Calling 'run_query'");
    gncrpc_run_query_1 (&args, &ret, be->client);
      
    rpcend_free_gncquery (&gq);
  }

  if (ret.error != 0) {
    xaccBackendSetError (&be->be, ret.error);
    LEAVE ("be=%p, q=%p, query_1", be, q);
    return;
  }

  /* Foreach transaction, figure out if it's in the engine or not.  If
   * it's not in the engine, or if the version implies an updated
   * version, then add it to the list of txns to download.  Then
   * download all the 'updated' txns and add them to the engine
   */

  {
    gnc_vers_list *old = NULL, **endold = &old;
    gnc_vers_list *new = NULL, **endnew = &new;
    gnc_vers_list *verslist;

    /* Figure out which transactions we need to add */
    for (verslist = ret.txnlist; verslist != NULL; verslist = verslist->next) {
      if (rpcend_would_add_txn (verslist) < 0) {
	*endnew = verslist;
	endnew = &(verslist->next);
      } else {
	*endold = verslist;
	endold = &(verslist->next);
      }
    }

    *endnew = NULL;

    memset (&txns_ret, 0, sizeof (txns_ret));
    if (new) {
      /* Now grab the updated/new transactions */
      memcpy (txns_args.backend, (char *)&be, sizeof(be));
      txns_args.guids = new;
      PINFO ("Calling 'get_txns'");
      gncrpc_get_txns_1 (&txns_args, &txns_ret, be->client);

      /* And now reconnect the new and old txnlist and put it back in
       * the results so that freeres will destroy it all
       */
    }
    *endold = new;
    ret.txnlist = old;
  }

  /* Free the results from the first RPC query */
  CLNT_FREERES (be->client, (xdrproc_t)xdr_gncrpc_query_ret, (caddr_t)&ret);

  /* Make sure this RPC was ok */
  if (txns_ret.error != 0) {
    xaccBackendSetError (&be->be, txns_ret.error);
    LEAVE ("be=%p, q=%p, get_txns", be, q);
    return;
  }

  /* Suspend events */
  gnc_engine_suspend_events();
  rpcendDisable(be);

  /* Now, add all the transactions to the engine */
  rpcend_add_gnctransaction_list (be, txns_ret.txnlist);

  /* Resume events */
  rpcendEnable(be);
  gnc_engine_resume_events();

  /* And free the results */
  CLNT_FREERES (be->client, (xdrproc_t)xdr_gncrpc_get_txns_ret, (caddr_t)&txns_ret);
  LEAVE ("be=%p, q=%p", be, q);
}

static void rpcend_sync (Backend *bend, AccountGroup *acctgrp)
{
  RPCBackend *be = (RPCBackend *)bend;
  gncrpc_sync1_args args1;
  gncrpc_sync1_ret ret1;
  gncrpc_sync2_args args2;
  int ret2 = -1;
  gnc_commodity_table *ct = gnc_book_get_commodity_table (be->book);
  VERIFY_BEV (be);
  ENTER ("be=%p, ag=%p", be, acctgrp);

  memset (&args1, 0, sizeof (args1));
  memset (&ret1, 0, sizeof (ret1));
  memcpy (args1.backend, (char *)&be, sizeof(be));
  args1.commodities = rpcend_build_gnccommoditylist (ct, FALSE);
  args1.acctlist = rpcend_build_gncacct_verslist (acctgrp, FALSE);
  args1.txnlist = rpcend_build_gnctxn_verslist (acctgrp, FALSE);

  /* Call RPC */
  gncrpc_sync1_1 (&args1, &ret1, be->client);

  /* Free the arguments of request1 */
  rpcend_free_gnccommoditylist (args1.commodities, FALSE);
  rpcend_free_verslist (args1.txnlist, FALSE);
  rpcend_free_verslist (args1.acctlist, FALSE);

  if (ret1.error != 0) {
    xaccBackendSetError (&be->be, ret1.error);
    CLNT_FREERES (be->client, (xdrproc_t)xdr_gncrpc_sync1_ret, (caddr_t)&ret1);
    LEAVE ("be=%p, ag=%p, sync1", be, acctgrp);
    return;
  }
  
  /* XXX: Do we need to update any version numbers here? */

  /*
   * Now, add the accounts and transactions that the server sent us,
   * and send the server the accounts and transactions that the server
   * requested
   */

  /* Suspend events */
  gnc_engine_suspend_events();
  rpcendDisable(be);

  /* Add all the commoditities, accounts, and transactions to the engine */
  rpcend_add_gnccommoditylist (be, ret1.commodities);
  rpcend_add_gncacctlist (be, acctgrp, ret1.acctlist);
  rpcend_add_gnctransaction_list (be, ret1.txnlist);

  /* Then build the response to send back to the server */
  memset (&args2, 0, sizeof (args2));
  memcpy (args2.backend, (char *)&be, sizeof(be));
  args2.acctlist = rpcend_build_gncacctlist_list (acctgrp,
						  ret1.send_acctlist);
  args2.txnlist = rpcend_build_gnctxnlist_list (acctgrp,
						ret1.send_txnlist);

  /* Free the first results */
  CLNT_FREERES (be->client, (xdrproc_t)xdr_gncrpc_sync1_ret, (caddr_t)&ret1);

  /* set everything as saved */
  xaccGroupMarkSaved (acctgrp);

  /* Resume events */
  rpcendEnable(be);
  gnc_engine_resume_events();

  /* Now send this back to the server */
  gncrpc_sync2_1 (&args2, &ret2, be->client);

  /* Free the arguments to request 2 */
  rpcend_free_gnctxnlist (args2.txnlist);
  rpcend_free_gncacctlist (args2.acctlist);

  /* And return */
  xaccBackendSetError (&be->be, ret2);
  LEAVE ("be=%p, ag=%p", be, acctgrp);
  return;
}

static gboolean rpcend_events_pending (Backend *bend)
{
  RPCBackend *be = (RPCBackend *)bend;
  VERIFY_BE (be, FALSE);

  /* The events flag can only be cleared from within this thread of
   * control.  So, the worst thing that can happen here is that we
   * have a false-negative.
   */
  return be->events;
}

static gboolean rpcend_process_events (Backend *bend)
{
  RPCBackend *be = (RPCBackend *)bend;
  gboolean changed = FALSE;
  VERIFY_BE (be, FALSE);

  pthread_mutex_lock (&(be->eventlock));
  if (!be->events) {
    pthread_mutex_unlock (&(be->eventlock));
    return FALSE;
  }

  /* Yep, there are events.  Copy off the event information, then
   * unlock the Backend event handler.  Then go off and process the
   * events.  Note that one of the events could be "disconnected from
   * server", so check for that first!
   */

  /* Process any waiting events */
  /* XXX */
  return changed;
}

static void rpcend_book_begin (GNCBook *book, const char *book_id, 
			       gboolean ignore_lock, gboolean create)
{
  RPCBackend *be;
  char *url, *start, *end, *rest;

  if (!book) return;
  be = (RPCBackend *) xaccGNCBookGetBackend (book);
  VERIFY_BEV (be);

  ENTER("be=%p, id=%s, ignore=%s, create=%s", be,
        book_id ? book_id : "",
	(ignore_lock == TRUE ? "true" : "false"),
	(create == TRUE ? "true" : "false"));

  /* close any dangling sessions from before and then reinitialize */
  rpcend_book_end ((Backend *)be);
  rpcendInit (be);

  /* Remember my book */
  be->book = book;

  /* Parse the book_id for the hostname and db name.
   * The expected URL format is:
   * rpc://host[:port]/db_name
   */
  if (strncmp (book_id, "rpc://", 6)) {
    xaccBackendSetError (&be->be, ERR_RPC_BAD_URL);
    LEAVE ("Not an RPC URL?");
    return;
  }
  url = g_strdup(book_id);
  start = url + 6;
  rest = strchr (start, '/');
  if (!rest || *rest == '\0') {
    xaccBackendSetError (&be->be, ERR_RPC_BAD_URL);
    g_free (url);
    LEAVE ("cannot find a path after host[:port]");
    return;
  }
  *rest = '\0';
  end = strchr (start, ':');
  if (end) {
    /* end would have the port number */
    *end = '\0';
    be->portNum = g_strdup (end+1);
  }
  be->hostname = g_strdup (start);
  start = rest+1;
  if (*start == '\0') {
    xaccBackendSetError (&be->be, ERR_RPC_BAD_URL);
    g_free (url);
    LEAVE ("tailing slash but no path after host[:port]");
    return;
  }

  /* dbname is the last thing before any url-encoded data */
  be->dbName = g_strdup (start);

  /* I would parse url-encoded data here, but I don't have any */
  /* XXX */

  /* make the connection */
  rpcendLogin (be);

  /* Free the URL */
  g_free (url);

  if (be->xprt == NULL) 
    return;

  /* Call the remote procedure to open the book */
  {
    gncrpc_book_begin_args args;
    int res = 0;

    memset (&args, 0, sizeof (args));
    memcpy (args.book, (char *)&book, sizeof (book));
    memcpy (args.backend, (char *)&be, sizeof (be));
    args.book_id = be->dbName;
    args.ignore_lock = ignore_lock;
    args.create = create;      

    gncrpc_book_begin_1 (&args, &res, be->client);

    if (res != 0) {
      RpcClose (be->sock);
      xaccBackendSetError (&be->be, res);
      LEAVE ("begin");
      return;
    }
  }

  /* Setup callbacks */
  rpcendEnable (be);
  be->be.book_end = rpcend_book_end;
  be->be.book_load = rpcend_book_load;
  be->be.account_begin_edit = rpcend_account_begin_edit;
  be->be.account_commit_edit = rpcend_account_commit_edit;
  be->be.trans_begin_edit = rpcend_trans_begin_edit;
  be->be.trans_commit_edit = rpcend_trans_commit_edit;
  be->be.trans_rollback_edit = rpcend_trans_rollback_edit;
  be->be.run_query = rpcend_run_query;
  be->be.sync = rpcend_sync;
  be->be.events_pending = rpcend_events_pending;
  be->be.process_events = rpcend_process_events;

  LEAVE("be=%p, id=%s", be, book_id ? book_id : "");
}

static void
rpcendInit (RPCBackend *be)
{
  memset (be, 0, sizeof (*be));

  /* The only callback that should work is Begin */
  be->be.book_begin = rpcend_book_begin;

  rpcendDisable (be);
  be->be.last_err = ERR_BACKEND_NO_ERR;

  /* RPC specific data */
  be->magic = RPCEND_MAGIC;

  /* Initialize the event mutex */
  pthread_mutex_init (&(be->eventlock), NULL);
}

Backend *
rpcendNew (void)
{
  RPCBackend *be;
  be = (RPCBackend *) g_malloc (sizeof (*be));
  rpcendInit (be);
  
  return (Backend *) be;
}

