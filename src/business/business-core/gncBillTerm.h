/*
 * gncBillTerm.h -- the Gnucash Billing Term interface
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BILLTERM_H_
#define GNC_BILLTERM_H_

typedef struct _gncBillTerm GncBillTerm;

#include "gnc-numeric.h"
#include "gnc-book.h"

#define GNC_BILLTERM_MODULE_NAME "gncBillTerm"

/*
 *  How to interpret the amount.
 * You can interpret it as a VALUE or a PERCENT.
 */
typedef enum {
  GNC_TERM_TYPE_DAYS = 1,
  GNC_TERM_TYPE_PROXIMO,
} GncBillTermType;

/* Create/Destroy Functions */
GncBillTerm * gncBillTermCreate (GNCBook *book);
void gncBillTermDestroy (GncBillTerm *term);

/* Set Functions */
void gncBillTermSetName (GncBillTerm *term, const char *name);
void gncBillTermSetDescription (GncBillTerm *term, const char *name);
void gncBillTermSetType (GncBillTerm *term, GncBillTermType type);
void gncBillTermSetDueDays (GncBillTerm *term, gint days);
void gncBillTermSetDiscountDays (GncBillTerm *term, gint days);
void gncBillTermSetDiscount (GncBillTerm *term, gnc_numeric discount);
void gncBillTermSetCutoff (GncBillTerm *term, gint cutoff);

void gncBillTermIncRef (GncBillTerm *term);
void gncBillTermDecRef (GncBillTerm *term);

void gncBillTermChanged (GncBillTerm *term);
void gncBillTermCommitEdit (GncBillTerm *term);

/* Get Functions */
GncBillTerm *gncBillTermLookup (GNCBook *book, const GUID *guid);
GncBillTerm *gncBillTermLookupByName (GNCBook *book, const char *name);
GList * gncBillTermGetTerms (GNCBook *book);

const GUID *gncBillTermGetGUID (GncBillTerm *term);
GNCBook *gncBillTermGetBook (GncBillTerm *term);
const char *gncBillTermGetName (GncBillTerm *term);
const char *gncBillTermGetDescription (GncBillTerm *term);
GncBillTermType gncBillTermGetType (GncBillTerm *term);
gint gncBillTermGetDueDays (GncBillTerm *term);
gint gncBillTermGetDiscountDays (GncBillTerm *term);
gnc_numeric gncBillTermGetDiscount (GncBillTerm *term);
gint gncBillTermGetCutoff (GncBillTerm *term);

gboolean gncBillTermIsDirty (GncBillTerm *term);

GncBillTerm *gncBillTermGetParent (GncBillTerm *term);
GncBillTerm *gncBillTermReturnChild (GncBillTerm *term, gboolean make_new);
#define gncBillTermGetChild(t) gncBillTermReturnChild((t),FALSE)
gint64 gncBillTermGetRefcount (GncBillTerm *term);

int gncBillTermCompare (GncBillTerm *a, GncBillTerm *b);

#endif /* GNC_BILLTERM_H_ */
