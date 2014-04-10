/*
 * gncTaxTableP.h -- the Gnucash Tax Table interface: private interface
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_TAXTABLEP_H_
#define GNC_TAXTABLEP_H_

#include "gncTaxTable.h"

gboolean gncTaxTableRegister (void);

void gncTaxTableSetGUID (GncTaxTable *table, const GUID *guid);
void gncTaxTableSetParent (GncTaxTable *table, GncTaxTable *parent);
void gncTaxTableSetChild (GncTaxTable *table, GncTaxTable *child);
void gncTaxTableSetRefcount (GncTaxTable *table, gint64 refcount);
void gncTaxTableMakeInvisible (GncTaxTable *table);

gboolean gncTaxTableGetInvisible (GncTaxTable *table);

#endif /* GNC_TAXTABLEP_H_ */
