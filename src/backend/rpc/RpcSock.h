/*
 * FILE:
 * RpcSock.h
 *
 * FUNCTION:
 * Implements the RPC Socket connection
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#ifndef __RPC_SOCK_H
#define __RPC_SOCK_H

#include "xprt_thrd.h"
#include "RpcBackend.h"

#define RPCEND_PORT	11207

typedef struct _rpcend_sock RPCSock;

int RpcConnect (char *hostname, unsigned short port, RPCSock **sock);
int RpcClose (RPCSock *sock);

int RpcCreateListener (unsigned short port, RPCSock **sock);
int RpcListen (RPCSock *sock, int val);
int RpcAccept (RPCSock *master, RPCSock **client);

int RpcTransport (RPCSock *sock, void (*myClose)(void *arg), void *cb_arg,
		  TXPRT **xprt);
void * RpcGetData (RPCSock *sock);

#endif /* __RPC_SOCK_H */
