/*
 * gncEntryP.h -- the Core Busines Entry Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ENTRYP_H_
#define GNC_ENTRYP_H_

#include "gncEntry.h"

gboolean gncEntryRegister (void);
void gncEntrySetGUID (GncEntry *entry, const GUID *guid);
void gncEntrySetOrder (GncEntry *entry, GncOrder *order);
void gncEntrySetInvoice (GncEntry *entry, GncInvoice *invoice);
void gncEntrySetBill (GncEntry *entry, GncInvoice *bill);
void gncEntrySetDirty (GncEntry *entry, gboolean dirty);

#endif /* GNC_ENTRYP_H_ */
