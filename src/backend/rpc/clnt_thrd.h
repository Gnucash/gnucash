/*
 * clnt_thrd.h -- Threaded, Multiplexed, RPC/TCP Client
 *
 * Written By:	Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef RPC_CLNT_THRD_H
#define RPC_CLNT_THRD_H

#include <rpc/clnt.h>
#include "xprt_thrd.h"

CLIENT *clntthrd_create (TXPRT *transport, u_long prog, u_long vers);

#endif /* RPC_CLNT_THRD_H */
