/*
 * gncOwner.h -- Business Interface:  Object OWNERs
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_OWNER_H_
#define GNC_OWNER_H_

typedef struct gnc_owner_s GncOwner;

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

struct gnc_owner_s {
  GncOwnerType		type;
  union {
    gpointer		undefined;
    GncCustomer *	customer;
    GncJob *		job;
    GncVendor *		vendor;
  } owner;
};

void gncOwnerInitUndefined (GncOwner *owner, gpointer obj);
void gncOwnerInitCustomer (GncOwner *owner, GncCustomer *customer);
void gncOwnerInitJob (GncOwner *owner, GncJob *job);
void gncOwnerInitVendor (GncOwner *owner, GncVendor *vendor);

GncOwnerType gncOwnerGetType (const GncOwner *owner);
gpointer gncOwnerGetUndefined (const GncOwner *owner);
GncCustomer * gncOwnerGetCustomer (const GncOwner *owner);
GncJob * gncOwnerGetJob (const GncOwner *owner);
GncVendor * gncOwnerGetVendor (const GncOwner *owner);

void gncOwnerCopy (const GncOwner *src, GncOwner *dest);
gboolean gncOwnerEqual (const GncOwner *a, const GncOwner *b);

const char * gncOwnerGetName (GncOwner *owner);

#endif /* GNC_OWNER_H_ */
