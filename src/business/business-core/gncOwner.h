/*
 * gncOwner.h -- Business Interface:  Object OWNERs
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_OWNER_H_
#define GNC_OWNER_H_

#include "gncCustomer.h"
#include "gncJob.h"
#include "gncVendor.h"

typedef enum {
  GNC_OWNER_NONE,
  GNC_OWNER_UNDEFINED,
  GNC_OWNER_CUSTOMER,
  GNC_OWNER_JOB,
  GNC_OWNER_VENDOR
} GncOwnerType;

typedef struct gnc_owner_s {
  GncOwnerType		type;
  union {
    gpointer		undefined;
    GncCustomer *	customer;
    GncJob *		job;
    GncVendor *		vendor;
  } owner;
} GncOwner;

void gncOwnerInitUndefined (GncOwner *owner, gpointer obj);
void gncOwnerInitCustomer (GncOwner *owner, GncCustomer *customer);
void gncOwnerInitJob (GncOwner *owner, GncJob *job);
void gncOwnerInitVendor (GncOwner *owner, GncVendor *vendor);

GncOwnerType gncOwnerGetType (GncOwner *owner);
gpointer gncOwnerGetUndefined (GncOwner *owner);
GncCustomer * gncOwnerGetCustomer (GncOwner *owner);
GncJob * gncOwnerGetJob (GncOwner *owner);
GncVendor * gncOwnerGetVendor (GncOwner *owner);

void gncOwnerCopy (const GncOwner *src, GncOwner *dest);

const char * gncOwnerGetName (GncOwner *owner);

#endif /* GNC_OWNER_H_ */
