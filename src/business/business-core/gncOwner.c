/*
 * gncOwner.c -- Business Interface:  Object OWNERs
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>		/* for memcpy() */

#include "gncOwner.h"

void gncOwnerInitUndefined (GncOwner *owner, gpointer obj)
{
  if (!owner) return;
  owner->type = GNC_OWNER_UNDEFINED;
  owner->owner.undefined = obj;
}

void gncOwnerInitCustomer (GncOwner *owner, GncCustomer *customer)
{
  if (!owner) return;
  owner->type = GNC_OWNER_CUSTOMER;
  owner->owner.customer = customer;
}

void gncOwnerInitJob (GncOwner *owner, GncJob *job)
{
  if (!owner) return;
  owner->type = GNC_OWNER_JOB;
  owner->owner.job = job;
}

void gncOwnerInitVendor (GncOwner *owner, GncVendor *vendor)
{
  if (!owner) return;
  owner->type = GNC_OWNER_VENDOR;
  owner->owner.vendor = vendor;
}

GncOwnerType gncOwnerGetType (GncOwner *owner)
{
  if (!owner) return GNC_OWNER_NONE;
  return owner->type;
}

gpointer gncOwnerGetUndefined (GncOwner *owner)
{
  if (!owner) return NULL;
  if (owner->type != GNC_OWNER_UNDEFINED) return NULL;
  return owner->owner.undefined;
}

GncCustomer * gncOwnerGetCustomer (GncOwner *owner)
{
  if (!owner) return NULL;
  if (owner->type != GNC_OWNER_CUSTOMER) return NULL;
  return owner->owner.customer;
}

GncJob * gncOwnerGetJob (GncOwner *owner)
{
  if (!owner) return NULL;
  if (owner->type != GNC_OWNER_JOB) return NULL;
  return owner->owner.job;
}

GncVendor * gncOwnerGetVendor (GncOwner *owner)
{
  if (!owner) return NULL;
  if (owner->type != GNC_OWNER_VENDOR) return NULL;
  return owner->owner.vendor;
}

void gncOwnerCopy (const GncOwner *src, GncOwner *dest)
{
  if (!src || !dest) return;
  if (src == dest) return;
  memcpy (dest, src, sizeof (*dest));
}

const char * gncOwnerGetName (GncOwner *owner)
{
  if (!owner) return NULL;
  switch (owner->type) {
  case GNC_OWNER_NONE:
  case GNC_OWNER_UNDEFINED:
  default:
    return NULL;
  case GNC_OWNER_CUSTOMER:
    return gncCustomerGetName (owner->owner.customer);
  case GNC_OWNER_JOB:
    return gncJobGetName (owner->owner.job);
  case GNC_OWNER_VENDOR:
    return gncVendorGetName (owner->owner.vendor);
  }
}


