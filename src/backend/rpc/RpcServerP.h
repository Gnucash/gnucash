/*
 * FILE:
 * RpcServerP.h
 *
 * FUNCTION:
 * Gnucash RPC Server Private Header
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#ifndef RPC_SERVERP_H
#define RPC_SERVERP_H

#include "xprt_thrd.h"

#include "RpcServer.h"
#include "RpcSock.h"
#include "gncRpc.h"
#include "gnc-book.h"

typedef struct _gncrpc_clist {
  GList *clist;
} GncRPCClist;

struct _gncrpc_svc {
  TXPRT *	xprt;		/* Transport */
  RPCSock *	sock;		/* Socket */

  GNCBook *	book;		/* My Client's Book */
  CLIENT *	clnt;		/* Client's Callback Object */

  GncRPCClist *	clist;		/* Client List */
};

#endif /* __RPC_SERVERP_H */
