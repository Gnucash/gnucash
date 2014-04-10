/*
 * FILE:
 * RpcServer.h
 *
 * FUNCTION:
 * Implements the Gnucash RPC server.
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#ifndef RPC_SERVER_H
#define RPC_SERVER_H

typedef struct _gncrpc_svc GncRpcSvc;

int rpc_server_run (unsigned short port);

#endif /* __RPC_SERVER_H */
