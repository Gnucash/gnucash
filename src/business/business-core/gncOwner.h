/*
 * gncOwner.h -- Business Interface:  Object OWNERs
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_OWNER_H_
#define GNC_OWNER_H_

typedef struct gnc_owner_s GncOwner;

#define GNC_OWNER_MODULE_NAME	"gncOwner"

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
int gncOwnerCompare (const GncOwner *a, const GncOwner *b);

const char * gncOwnerGetName (GncOwner *owner);

/* Get the GUID of the immediate owner */
const GUID * gncOwnerGetGUID (GncOwner *owner);

/*
 * Get the "parent" Owner or GUID thereof.  The "parent" owner
 * is the Customer or Vendor, or the Owner of a Job
 */
GncOwner * gncOwnerGetEndOwner (GncOwner *owner);
const GUID * gncOwnerGetEndGUID (GncOwner *owner);

#define OWNER_TYPE	"type"
#define OWNER_CUSTOMER	"customer"
#define OWNER_JOB	"job"
#define OWNER_VENDOR	"vendor"
#define OWNER_GUID	"guid"
#define OWNER_PARENT	"parent"
#define OWNER_PARENTG	"parent-guid"
#define OWNER_NAME	"name"

/*
 * These two functions are mainly for the convenience of scheme code.
 * Normal C code has no need to ever use these two functions, and rather
 * can just use a GncOwner directly and just pass around a pointer to it.
 */
GncOwner * gncOwnerCreate (void);
void gncOwnerDestroy (GncOwner *owner);

#endif /* GNC_OWNER_H_ */
