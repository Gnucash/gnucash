/********************************************************************\
 * gncCustomer.h -- the Core Customer Interface                     *
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
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_CUSTOMER_H_
#define GNC_CUSTOMER_H_

typedef struct _gncCustomer GncCustomer;

#include "qofbook.h"
#include "qofid.h"
#include "qofinstance.h"
#include "gncAddress.h"
#include "gncBillTerm.h"
#include "gncTaxTable.h"
#include "gncJob.h"

#include "gnc-numeric.h"
#include "kvp_frame.h"

#define GNC_CUSTOMER_MODULE_NAME "gncCustomer"

/* Create/Destroy Functions */

GncCustomer *gncCustomerCreate (QofBook *book);
void gncCustomerDestroy (GncCustomer *customer);

/* Set Functions */

void gncCustomerSetID (GncCustomer *customer, const char *id);
void gncCustomerSetName (GncCustomer *customer, const char *name);
void gncCustomerSetNotes (GncCustomer *customer, const char *notes);
void gncCustomerSetTerms (GncCustomer *customer, GncBillTerm *term);
void gncCustomerSetTaxIncluded (GncCustomer *customer, GncTaxIncluded taxincl);
void gncCustomerSetActive (GncCustomer *customer, gboolean active);
void gncCustomerSetDiscount (GncCustomer *customer, gnc_numeric discount);
void gncCustomerSetCredit (GncCustomer *customer, gnc_numeric credit);
void gncCustomerSetCurrency (GncCustomer *customer, gnc_commodity *currency);

void gncCustomerSetTaxTableOverride (GncCustomer *customer, gboolean override);
void gncCustomerSetTaxTable (GncCustomer *customer, GncTaxTable *table);

void gncCustomerAddJob (GncCustomer *customer, GncJob *job);
void gncCustomerRemoveJob (GncCustomer *customer, GncJob *job);

void gncCustomerBeginEdit (GncCustomer *customer);
void gncCustomerCommitEdit (GncCustomer *customer);

/* Get Functions */

const char * gncCustomerGetID (GncCustomer *customer);
const char * gncCustomerGetName (GncCustomer *customer);
GncAddress * gncCustomerGetAddr (GncCustomer *customer);
GncAddress * gncCustomerGetShipAddr (GncCustomer *customer);
const char * gncCustomerGetNotes (GncCustomer *customer);
GncBillTerm * gncCustomerGetTerms (GncCustomer *customer);
GncTaxIncluded gncCustomerGetTaxIncluded (GncCustomer *customer);
gboolean gncCustomerGetActive (GncCustomer *customer);
gnc_numeric gncCustomerGetDiscount (GncCustomer *customer);
gnc_numeric gncCustomerGetCredit (GncCustomer *customer);
gnc_commodity * gncCustomerGetCurrency (GncCustomer *customer);

gboolean gncCustomerGetTaxTableOverride (GncCustomer *customer);
GncTaxTable* gncCustomerGetTaxTable (GncCustomer *customer);

KvpFrame *gncCustomerGetSlots (GncCustomer *customer);
GList * gncCustomerGetJoblist (GncCustomer *customer, gboolean show_all);

GUID gncCustomerRetGUID (GncCustomer *customer);
GncCustomer * gncCustomerLookupDirect (GUID guid, QofBook *book);

GncCustomer * gncCustomerLookup (QofBook *book, const GUID *guid);

gboolean gncCustomerIsDirty (GncCustomer *customer);
int gncCustomerCompare (GncCustomer *a, GncCustomer *b);

#define CUSTOMER_ID	"id"
#define CUSTOMER_NAME	"name"
#define CUSTOMER_ADDR	"addr"
#define CUSTOMER_SHIPADDR	"shipaddr"

/* deprecated functions, should be removed */
#define gncCustomerGetGUID(x) qof_instance_get_guid(QOF_INSTANCE(x))
#define gncCustomerGetBook(x) qof_instance_get_book(QOF_INSTANCE(x))

#endif /* GNC_CUSTOMER_H_ */
