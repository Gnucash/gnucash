/*
 * QueryObject.h -- Private API for registering queriable Gnucash objects
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef GNC_QUERYOBJECTP_H
#define GNC_QUERYOBJECTP_H

#include "QueryObject.h"

void gncQueryObjectInit(void);
void gncQueryObjectShutdown (void);

QuerySort gncQueryObjectDefaultSort (GNCIdTypeConst obj_name);

#endif /* GNC_QUERYOBJECTP_H */
