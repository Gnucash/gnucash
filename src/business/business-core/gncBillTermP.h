/*
 * gncBillTermP.h -- the Gnucash Billing Term interface: private interface
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BILLTERMP_H_
#define GNC_BILLTERMP_H_

#include "gncBillTerm.h"

gboolean gncBillTermRegister (void);

void gncBillTermSetGUID (GncBillTerm *term, const GUID *guid);
void gncBillTermSetParent (GncBillTerm *term, GncBillTerm *parent);
void gncBillTermSetChild (GncBillTerm *term, GncBillTerm *child);
void gncBillTermSetRefcount (GncBillTerm *term, gint64 refcount);
void gncBillTermMakeInvisible (GncBillTerm *term);

gboolean gncBillTermGetInvisible (GncBillTerm *term);

#endif /* GNC_BILLTERMP_H_ */
