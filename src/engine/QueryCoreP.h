/*
 * QueryCoreP.h -- Internal API for providing core Query data types
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef GNC_QUERYCOREP_H
#define GNC_QUERYCOREP_H

#include "QueryCore.h"

/* Initalize the Query Core registry and install the default type handlers */
void gncQueryCoreInit(void);
void gncQueryCoreShutdown (void);

/* Lookup functions */
QueryPredicate gncQueryCoreGetPredicate (char const *type);
QueryCompare gncQueryCoreGetCompare (char const *type);
QueryPredicateCopy gncQueryCoreGetCopy (char const *type);
QueryPredDataFree gncQueryCoreGetPredFree (char const *type);

#endif /* GNC_QUERYCOREP_H */
