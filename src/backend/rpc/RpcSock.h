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

#ifndef RPC_SOCK_H
#define RPC_SOCK_H

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
