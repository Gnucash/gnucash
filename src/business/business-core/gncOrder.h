/*
 * gncOrder.h -- the Core Business Order Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ORDER_H_
#define GNC_ORDER_H_

typedef struct _gncOrder GncOrder;

#include "gnc-book.h"
#include "gncEntry.h"
#include "gncOwner.h"

#define GNC_ORDER_MODULE_NAME "gncOrder"

/* Create/Destroy Functions */

GncOrder *gncOrderCreate (GNCBook *book);
void gncOrderDestroy (GncOrder *order);

/* Set Functions */

void gncOrderSetID (GncOrder *order, const char *id);
void gncOrderSetOwner (GncOrder *order, GncOwner *owner);
void gncOrderSetDateOpened (GncOrder *order, Timespec *date);
void gncOrderSetDateClosed (GncOrder *order, Timespec *date);
void gncOrderSetNotes (GncOrder *order, const char *notes);
void gncOrderSetActive (GncOrder *order, gboolean active);

/* Add an Entry to the Order */
void gncOrderAddEntry (GncOrder *order, GncEntry *entry);
void gncOrderRemoveEntry (GncOrder *order, GncEntry *entry);

/* Get Functions */

GNCBook * gncOrderGetBook (GncOrder *order);
const GUID * gncOrderGetGUID (GncOrder *order);
const char * gncOrderGetID (GncOrder *order);
GncOwner * gncOrderGetOwner (GncOrder *order);
Timespec gncOrderGetDateOpened (GncOrder *order);
Timespec gncOrderGetDateClosed (GncOrder *order);
const char * gncOrderGetNotes (GncOrder *order);
gboolean gncOrderGetActive (GncOrder *order);

/* Get the list Entries */
GList * gncOrderGetEntries (GncOrder *order);

GncOrder * gncOrderLookup (GNCBook *book, const GUID *guid);
gboolean gncOrderIsDirty (GncOrder *order);
void gncOrderBeginEdit (GncOrder *order);
void gncOrderCommitEdit (GncOrder *order);
int gncOrderCompare (GncOrder *a, GncOrder *b);

#define ORDER_GUID	"guid"
#define ORDER_ID	"id"
#define ORDER_OWNER	"owner"
#define ORDER_OPENED	"date_opened"
#define ORDER_CLOSED	"date_closed"
#define ORDER_NOTES	"notes"

#endif /* GNC_ORDER_H_ */
