/*
 * gncBusiness.h -- Business Helper Functions
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BUSINESS_H_
#define GNC_BUSINESS_H_

#include <glib.h>

#include "qofbook.h"
#include "qofid.h"

typedef struct _gncBookInfo {
  gboolean	is_dirty;
} GncBookInfo;


#define gncBusinessForeach(book,mod_name,cb,user_data) \
  qof_entity_foreach(qof_book_get_entity_table(book), mod_name, cb, user_data)


void gncBusinessCreate (QofBook *book, QofIdType mod_name);
void gncBusinessDestroy (QofBook *book, QofIdType mod_name);
gboolean gncBusinessIsDirty (QofBook *book, QofIdType mod_name);
void gncBusinessSetDirtyFlag (QofBook *book, QofIdType mod_name,
			      gboolean is_dirty);


#endif /* GNC_BUSINESS_H_ */
