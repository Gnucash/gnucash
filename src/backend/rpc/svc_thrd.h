/*
 * svc_thrd.h -- Threaded, Multiplexed, RPC/TCP Service
 *
 * Written By:	Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef RPC_SVC_THRD_H
#define RPC_SVC_THRD_H

#include <rpc/svc.h>
#include "xprt_thrd.h"

extern SVCXPRT *svcthrd_create (TXPRT *transport);

#endif /* RPC_SVC_THRD_H */
