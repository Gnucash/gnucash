/*
 * gncBusiness.h -- Business Helper Functions
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BUSINESS_H_
#define GNC_BUSINESS_H_

#include "gnc-book.h"
#include "qofobject.h"
#include "qofid.h"

typedef struct _gncBookInfo {
  GHashTable *	ht;
  gboolean	is_dirty;
} GncBookInfo;

void gncBusinessForeach (QofBook *book, QofIdType mod_name,
			 QofEntityForeachCB cb, gpointer user_data);

void gncBusinessCreate (QofBook *book, QofIdType mod_name);
void gncBusinessDestroy (QofBook *book, QofIdType mod_name);
gboolean gncBusinessIsDirty (QofBook *book, QofIdType mod_name);
void gncBusinessSetDirtyFlag (QofBook *book, QofIdType mod_name,
			      gboolean is_dirty);
void gncBusinessAddObject (QofBook *book, QofIdType mod_name,
			   gpointer obj, const GUID *guid);
void gncBusinessRemoveObject (QofBook *book, QofIdType mod_name,
			      const GUID *guid);


#endif /* GNC_BUSINESS_H_ */
