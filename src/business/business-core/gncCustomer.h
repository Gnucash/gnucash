/*
 * gncCustomer.h -- the Core Customer Interface
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_CUSTOMER_H_
#define GNC_CUSTOMER_H_

typedef struct _gncCustomer GncCustomer;

#include "gnc-book.h"
#include "gncAddress.h"
#include "gncBillTerm.h"
#include "gncTaxTable.h"
#include "gncJob.h"

#include "gnc-numeric.h"

#define GNC_CUSTOMER_MODULE_NAME "gncCustomer"

/* Create/Destroy Functions */

GncCustomer *gncCustomerCreate (GNCBook *book);
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
void gncCustomerSetCommodity (GncCustomer *customer, gnc_commodity *com);

void gncCustomerAddJob (GncCustomer *customer, GncJob *job);
void gncCustomerRemoveJob (GncCustomer *customer, GncJob *job);

void gncCustomerCommitEdit (GncCustomer *customer);

/* Get Functions */

GNCBook * gncCustomerGetBook (GncCustomer *customer);
const GUID * gncCustomerGetGUID (GncCustomer *customer);
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
gnc_commodity * gncCustomerGetCommodity (GncCustomer *customer);

GList * gncCustomerGetJoblist (GncCustomer *customer, gboolean show_all);

GUID gncCustomerRetGUID (GncCustomer *customer);
GncCustomer * gncCustomerLookupDirect (GUID guid, GNCBook *book);

GncCustomer * gncCustomerLookup (GNCBook *book, const GUID *guid);

gboolean gncCustomerIsDirty (GncCustomer *customer);
int gncCustomerCompare (GncCustomer *a, GncCustomer *b);

#define CUSTOMER_ID	"id"
#define CUSTOMER_NAME	"name"
#define CUSTOMER_ADDR	"addr"
#define CUSTOMER_SHIPADDR	"shipaddr"

#endif /* GNC_CUSTOMER_H_ */
