/*
 * gncTaxTable.h -- the Gnucash Tax Table interface
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_TAXTABLE_H_
#define GNC_TAXTABLE_H_

typedef struct _gncTaxTable GncTaxTable;
typedef struct _gncTaxTableEntry GncTaxTableEntry;

#include "gnc-numeric.h"
#include "gnc-book.h"
#include "Account.h"

#define GNC_TAXTABLE_MODULE_NAME "gncTaxTable"

/*
 *  How to interpret the amount.
 * You can interpret it as a VALUE or a PERCENT.
 */
typedef enum {
  GNC_TAX_TYPE_VALUE = 0,
  GNC_TAX_TYPE_PERCENT = 1,
  GNC_TAX_TYPE_NONE = 2,
} GncTaxType;

/* Create/Destroy Functions */
GncTaxTable * gncTaxTableCreate (GNCBook *book);
void gncTaxTableDestroy (GncTaxTable *table);
GncTaxTableEntry * gncTaxTableEntryCreate (void);
void gncTaxTableEntryDestroy (GncTaxTableEntry *entry);

/* Set Functions */
void gncTaxTableSetName (GncTaxTable *table, const char *name);
void gncTaxTableIncRef (GncTaxTable *table);
void gncTaxTableDecRef (GncTaxTable *table);

void gncTaxTableEntrySetAccount (GncTaxTableEntry *entry, Account *account);
void gncTaxTableEntrySetType (GncTaxTableEntry *entry, GncTaxType type);
void gncTaxTableEntrySetAmount (GncTaxTableEntry *entry, gnc_numeric amount);

void gncTaxTableAddEntry (GncTaxTable *table, GncTaxTableEntry *entry);
void gncTaxTableRemoveEntry (GncTaxTable *table, GncTaxTableEntry *entry);

void gncTaxTableChanged (GncTaxTable *table);
void gncTaxTableCommitEdit (GncTaxTable *table);

/* Get Functions */
GncTaxTable *gncTaxTableLookupByName (GNCBook *book, const char *name);
GList * gncTaxTableGetTables (GNCBook *book);

const GUID *gncTaxTableGetGUID (GncTaxTable *table);
GNCBook *gncTaxTableGetBook (GncTaxTable *table);
const char *gncTaxTableGetName (GncTaxTable *table);
GncTaxTable *gncTaxTableGetParent (GncTaxTable *table);
GncTaxTable *gncTaxTableGetChild (GncTaxTable *table, gboolean make_new);
#define gncTaxTableReturnChild(t) gncTaxTableGetChild((t),FALSE)
GList *gncTaxTableGetEntries (GncTaxTable *table);
gint64 gncTaxTableGetRefcount (GncTaxTable *table);

Account * gncTaxTableEntryGetAccount (GncTaxTableEntry *entry);
GncTaxType gncTaxTableEntryGetType (GncTaxTableEntry *entry);
gnc_numeric gncTaxTableEntryGetAmount (GncTaxTableEntry *entry);

int gncTaxTableCompare (GncTaxTable *a, GncTaxTable *b);
int gncTaxTableEntryCompare (GncTaxTableEntry *a, GncTaxTableEntry *b);

#endif /* GNC_TAXTABLE_H_ */
