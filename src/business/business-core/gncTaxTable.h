/*
 * gncTaxTable.h -- the Gnucash Tax Table interface
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_TAXTABLE_H_
#define GNC_TAXTABLE_H_

typedef struct _gncTaxTable GncTaxTable;
typedef struct _gncTaxTableEntry GncTaxTableEntry;
typedef struct _gncAccountValue GncAccountValue;

#include "gnc-numeric.h"
#include "gnc-book.h"
#include "date.h"
#include "Account.h"

#define GNC_TAXTABLE_MODULE_NAME "gncTaxTable"

/*
 * How to interpret the amount.
 * You can interpret it as a VALUE or a PERCENT.
 */
typedef enum {
  GNC_AMT_TYPE_VALUE = 1,
  GNC_AMT_TYPE_PERCENT
} GncAmountType;

/* How to interpret the TaxIncluded */
typedef enum {
  GNC_TAXINCLUDED_YES = 1,
  GNC_TAXINCLUDED_NO,
  GNC_TAXINCLUDED_USEGLOBAL,
} GncTaxIncluded;

const char * gncAmountTypeToString (GncAmountType type);
gboolean gncAmountStringToType (const char *str, GncAmountType *type);

const char * gncTaxIncludedTypeToString (GncTaxIncluded type);
gboolean gncTaxIncludedStringToType (const char *str, GncTaxIncluded *type);

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
void gncTaxTableEntrySetType (GncTaxTableEntry *entry, GncAmountType type);
void gncTaxTableEntrySetAmount (GncTaxTableEntry *entry, gnc_numeric amount);

void gncTaxTableAddEntry (GncTaxTable *table, GncTaxTableEntry *entry);
void gncTaxTableRemoveEntry (GncTaxTable *table, GncTaxTableEntry *entry);

void gncTaxTableChanged (GncTaxTable *table);
void gncTaxTableCommitEdit (GncTaxTable *table);

/* Get Functions */
GncTaxTable *gncTaxTableLookup (GNCBook *book, const GUID *guid);
GncTaxTable *gncTaxTableLookupByName (GNCBook *book, const char *name);
GList * gncTaxTableGetTables (GNCBook *book);

const GUID *gncTaxTableGetGUID (GncTaxTable *table);
GNCBook *gncTaxTableGetBook (GncTaxTable *table);
const char *gncTaxTableGetName (GncTaxTable *table);
GncTaxTable *gncTaxTableGetParent (GncTaxTable *table);
GncTaxTable *gncTaxTableReturnChild (GncTaxTable *table, gboolean make_new);
#define gncTaxTableGetChild(t) gncTaxTableReturnChild((t),FALSE)
GList *gncTaxTableGetEntries (GncTaxTable *table);
gint64 gncTaxTableGetRefcount (GncTaxTable *table);
Timespec gncTaxTableLastModified (GncTaxTable *table);

Account * gncTaxTableEntryGetAccount (GncTaxTableEntry *entry);
GncAmountType gncTaxTableEntryGetType (GncTaxTableEntry *entry);
gnc_numeric gncTaxTableEntryGetAmount (GncTaxTableEntry *entry);

int gncTaxTableCompare (GncTaxTable *a, GncTaxTable *b);
int gncTaxTableEntryCompare (GncTaxTableEntry *a, GncTaxTableEntry *b);


/************************************************/

struct _gncAccountValue {
  Account *	account;
  gnc_numeric	value;
};

/*
 * This will add value to the account-value for acc, creating a new
 * list object if necessary
 */
GList *gncAccountValueAdd (GList *list, Account *acc, gnc_numeric value);

/* Merge l2 into l1.  l2 is not touched. */
GList *gncAccountValueAddList (GList *l1, GList *l2);

/* return the total for this list */
gnc_numeric gncAccountValueTotal (GList *list);

/* Destroy a list of accountvalues */
void gncAccountValueDestroy (GList *list);


#endif /* GNC_TAXTABLE_H_ */
