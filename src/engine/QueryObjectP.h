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

const QueryObjectDef * gncQueryObjectGetParameter (GNCIdType obj_name,
						   const char *parameter);

QueryAccess gncQueryObjectGetParamaterGetter (GNCIdType obj_name,
					      const char *parameter);

QueryConvert gncQueryObjectGetConverter (GNCIdType from_obj,
					 GNCIdType to_obj);

QuerySort gncQueryObjectDefaultSort (GNCIdType obj_name);

#endif /* GNC_QUERYOBJECTP_H */
