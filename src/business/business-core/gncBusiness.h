/*
 * gncBusiness.h -- Business Helper Functions
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BUSINESS_H_
#define GNC_BUSINESS_H_

#include "gncObject.h"
#include "GNCId.h"

typedef struct _gncBookInfo {
  GHashTable *	ht;
  gboolean	is_dirty;
} GncBookInfo;

void gncBusinessForeach (GNCBook *book, GNCIdType mod_name,
			 foreachObjectCB cb, gpointer user_data);

void gncBusinessCreate (GNCBook *book, GNCIdType mod_name);
void gncBusinessDestroy (GNCBook *book, GNCIdType mod_name);
gboolean gncBusinessIsDirty (GNCBook *book, GNCIdType mod_name);
void gncBusinessSetDirtyFlag (GNCBook *book, GNCIdType mod_name,
			      gboolean is_dirty);
void gncBusinessAddObject (GNCBook *book, GNCIdType mod_name,
			   gpointer obj, const GUID *guid);
void gncBusinessRemoveObject (GNCBook *book, GNCIdType mod_name,
			      const GUID *guid);


#endif /* GNC_BUSINESS_H_ */
