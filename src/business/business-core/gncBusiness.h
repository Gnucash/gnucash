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

/* XXXX this is to be replaced by QofCollection shortly,
 * which should provide the dirty flag, as well as defualt 
 * for-each'es.
 * */
typedef struct _gncBookInfo {
  gboolean	is_dirty;
} GncBookInfo;


#define gncBusinessForeach(book,mod_name,cb,userdata) \
  qof_collection_foreach(qof_book_get_collection((book), (mod_name)), ((QofEntityForeachCB)(cb)), (userdata))

/** another temporary hack for entity lookup */

#define ELOOKUP(c_type) { \
  QofCollection *col; \
  if (!guid || !book) return NULL; \
  col = qof_book_get_collection (book, _GNC_MOD_NAME); \
  return (c_type *) qof_collection_lookup_entity (col, guid); \
}


void gncBusinessCreate (QofBook *book, QofIdType mod_name);
void gncBusinessDestroy (QofBook *book, QofIdType mod_name);
gboolean gncBusinessIsDirty (QofBook *book, QofIdType mod_name);
void gncBusinessSetDirtyFlag (QofBook *book, QofIdType mod_name,
			      gboolean is_dirty);


#endif /* GNC_BUSINESS_H_ */
