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

/** another temporary hack for entity lookup */

#define ELOOKUP(c_type) { \
  QofCollection *col; \
  if (!guid || !book) return NULL; \
  col = qof_book_get_collection (book, _GNC_MOD_NAME); \
  return (c_type *) qof_collection_lookup_entity (col, guid); \
}


#endif /* GNC_BUSINESS_H_ */
