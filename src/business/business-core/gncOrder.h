/*
 * gncOrder.h -- the Core Business Order Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ORDER_H_
#define GNC_ORDER_H_

typedef struct _gncOrder GncOrder;

typedef enum {
  GNC_ORDER_NONE = 0,
  GNC_ORDER_SALES = 1,
  GNC_ORDER_PURCHASE = 2
} GncOrderType;

#include "gncBusiness.h"
#include "gncJob.h"
#include "gncVendor.h"

#define GNC_ORDER_MODULE_NAME "gncOrder"

/* Create/Destroy Functions */

GncOrder *gncOrderCreate (GncBusiness *business, GncOrderType type);
void gncOrderDestroy (GncOrder *order);

/* Set Functions */

void gncOrderSetID (GncOrder *order, const char *id);
void gncOrderSetJob (GncOrder *order, GncJob *job);
void gncOrderSetVendor (GncOrder *order, GncVendor *vendor);
void gncOrderSetDateOpened (GncOrder *order, Timespec *date);
void gncOrderSetDateClosed (GncOrder *order, Timespec *date);
void gncOrderSetNotes (GncOrder *order, const char *notes);
void gncOrderSetActive (GncOrder *order, gboolean active);

/* Add an Entry to the Order */
void gncOrderAddEntry (GncOrder *order, GncEntry *entry);
void gncOrderRemoveEntry (GncOrder *order, GncEntry *entry);

/* Get Functions */

GncBusiness * gncOrderGetBusiness (GncOrder *order);
const GUID * gncOrderGetGUID (GncOrder *order);
const char * gncOrderGetID (GncOrder *order);
GncOrderType gncOrderGetType (GncOrder *order);
GncJob * gncOrderGetJob (GncOrder *order);
GncVendor * gncOrderGetVendor (GncOrder *order);
Timespec gncOrderGetDateOpened (GncOrder *order);
Timespec gncOrderGetDateClosed (GncOrder *order);
const char * gncOrderGetNotes (GncOrder *order);
gboolean gncOrderGetActive (GncOrder *order);

/* Get the list Entries */
GList * gncOrderGetEntries (GncOrder *order);

gboolean gncOrderIsDirty (GncOrder *order);
void gncOrderCommitEdit (GncOrder *order);

#endif /* GNC_ORDER_H_ */
