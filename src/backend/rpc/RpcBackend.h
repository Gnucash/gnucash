/*
 * FILE:
 * RpcBackend.h
 *
 * FUNCTION:
 * Implements the callbacks for the RPC (client) backend.
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#ifndef RPC_BACKEND_H
#define RPC_BACKEND_H

#include "BackendP.h"

typedef struct _rpcend RPCBackend;

/*
 * rpcendNew() creates a new RPC Backend
 */
Backend * rpcendNew (void);

#endif /* __RPC_BACKEND_H */
