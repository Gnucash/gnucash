/********************************************************************\
 * gncBillTerm.h -- the Gnucash Billing Term interface              *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BILLTERM_H_
#define GNC_BILLTERM_H_

typedef struct _gncBillTerm GncBillTerm;

#include "gnc-date.h"
#include "gnc-numeric.h"
#include "qofbook.h"
#include "qofid.h"
#include "qofinstance.h"
#include "gncBusiness.h"

#define GNC_ID_BILLTERM       "gncBillTerm"
#define GNC_IS_BILLTERM(obj)  (QOF_CHECK_TYPE((obj), GNC_ID_BILLTERM))
#define GNC_BILLTERM(obj)     (QOF_CHECK_CAST((obj), GNC_ID_BILLTERM, GncBillTerm))

/*
 *  How to interpret the amount.
 * You can interpret it as a VALUE or a PERCENT.
 */
typedef enum {
  GNC_TERM_TYPE_DAYS = 1,
  GNC_TERM_TYPE_PROXIMO,
} GncBillTermType;

/* Create/Destroy Functions */
GncBillTerm * gncBillTermCreate (QofBook *book);
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
void gncBillTermBeginEdit (GncBillTerm *term);
void gncBillTermCommitEdit (GncBillTerm *term);

/* Get Functions */

/** Return a pointer to the instance gncBillTerm that is identified
 *  by the guid, and is residing in the book. Returns NULL if the 
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncBillTerm * gncBillTermLookup (QofBook *book, const GUID *guid);
 */
#define gncBillTermLookup(book,guid)    \
       QOF_BOOK_LOOKUP_ENTITY((book),(guid),GNC_ID_BILLTERM, GncBillTerm)

GncBillTerm *gncBillTermLookupByName (QofBook *book, const char *name);
GList * gncBillTermGetTerms (QofBook *book);
KvpFrame* gncBillTermGetSlots (GncBillTerm *term);

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

/********************************************************/
/* functions to compute dates from Bill Terms           */

/* Compute the due date and discount dates from the post date */
Timespec gncBillTermComputeDueDate (GncBillTerm *term, Timespec post_date);
Timespec gncBillTermComputeDiscountDate (GncBillTerm *term, Timespec post_date);

/* deprecated */
#define gncBillTermGetGUID(x) qof_instance_get_guid (QOF_INSTANCE(x))

#endif /* GNC_BILLTERM_H_ */
