/*
 * gncCustomer.h -- the Core Customer Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_CUSTOMER_H_
#define GNC_CUSTOMER_H_

struct _gncCustomer;
typedef struct _gncCustomer GncCustomer;

#include "gncBusiness.h"
#include "gncAddress.h"
#include "gncJob.h"

#include "gnc-numeric.h"

#define GNC_CUSTOMER_MODULE_NAME "gncCustomer"

/* Create/Destroy Functions */

GncCustomer *gncCustomerCreate (GncBusiness *business);
void gncCustomerDestroy (GncCustomer *customer);

/* Set Functions */

void gncCustomerSetID (GncCustomer *customer, const char *id);
void gncCustomerSetName (GncCustomer *customer, const char *name);
void gncCustomerSetNotes (GncCustomer *customer, const char *notes);
void gncCustomerSetTerms (GncCustomer *customer, gint terms);
void gncCustomerSetTaxIncluded (GncCustomer *customer, gboolean taxincl);
void gncCustomerSetActive (GncCustomer *customer, gboolean active);
void gncCustomerSetDiscount (GncCustomer *customer, gnc_numeric discount);
void gncCustomerSetCredit (GncCustomer *customer, gnc_numeric credit);

void gncCustomerAddJob (GncCustomer *customer, GncJob *job);
void gncCustomerRemoveJob (GncCustomer *customer, GncJob *job);

void gncCustomerCommitEdit (GncCustomer *customer);

/* Get Functions */

GncBusiness * gncCustomerGetBusiness (GncCustomer *business);
const GUID * gncCustomerGetGUID (GncCustomer *customer);
const char * gncCustomerGetID (GncCustomer *customer);
const char * gncCustomerGetName (GncCustomer *customer);
GncAddress * gncCustomerGetAddr (GncCustomer *customer);
GncAddress * gncCustomerGetShipAddr (GncCustomer *customer);
const char * gncCustomerGetNotes (GncCustomer *customer);
gint gncCustomerGetTerms (GncCustomer *customer);
gboolean gncCustomerGetTaxIncluded (GncCustomer *customer);
gboolean gncCustomerGetActive (GncCustomer *customer);
gnc_numeric gncCustomerGetDiscount (GncCustomer *customer);
gnc_numeric gncCustomerGetCredit (GncCustomer *customer);

GList * gncCustomerGetJoblist (GncCustomer *customer, gboolean show_all);

gboolean gncCustomerIsDirty (GncCustomer *customer);

#endif /* GNC_CUSTOMER_H_ */
