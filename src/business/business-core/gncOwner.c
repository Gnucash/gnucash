/*
 * gncOwner.c -- Business Interface:  Object OWNERs
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>		/* for memcpy() */

#include "QueryObject.h"

#include "gncOwner.h"
#include "gncOwnerP.h"

#define _GNC_MOD_NAME	GNC_OWNER_MODULE_NAME

GncOwner * gncOwnerCreate (void)
{
  GncOwner *o = g_new0 (GncOwner, 1);
  o->type = GNC_OWNER_NONE;
  return o;
}

void gncOwnerDestroy (GncOwner *owner)
{
  if (!owner) return;
  g_free (owner);
}

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

GncOwnerType gncOwnerGetType (const GncOwner *owner)
{
  if (!owner) return GNC_OWNER_NONE;
  return owner->type;
}

gpointer gncOwnerGetUndefined (const GncOwner *owner)
{
  if (!owner) return NULL;
  if (owner->type != GNC_OWNER_UNDEFINED) return NULL;
  return owner->owner.undefined;
}

GncCustomer * gncOwnerGetCustomer (const GncOwner *owner)
{
  if (!owner) return NULL;
  if (owner->type != GNC_OWNER_CUSTOMER) return NULL;
  return owner->owner.customer;
}

GncJob * gncOwnerGetJob (const GncOwner *owner)
{
  if (!owner) return NULL;
  if (owner->type != GNC_OWNER_JOB) return NULL;
  return owner->owner.job;
}

GncVendor * gncOwnerGetVendor (const GncOwner *owner)
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

gboolean gncOwnerEqual (const GncOwner *a, const GncOwner *b)
{
  if (!a || !b) return FALSE;
  if (gncOwnerGetType (a) != gncOwnerGetType (b)) return FALSE;
  return (a->owner.undefined == b->owner.undefined);
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

const GUID * gncOwnerGetGUID (GncOwner *owner)
{
  if (!owner) return NULL;

  switch (owner->type) {
  case GNC_OWNER_NONE:
  case GNC_OWNER_UNDEFINED:
  default:
    return NULL;
  case GNC_OWNER_CUSTOMER:
    return gncCustomerGetGUID (owner->owner.customer);
  case GNC_OWNER_JOB:
    return gncJobGetGUID (owner->owner.job);
  case GNC_OWNER_VENDOR:
    return gncVendorGetGUID (owner->owner.vendor);
  }
}

GncOwner * gncOwnerGetEndOwner (GncOwner *owner)
{
  if (!owner) return NULL;
  switch (owner->type) {
  case GNC_OWNER_NONE:
  case GNC_OWNER_UNDEFINED:
  default:
    return NULL;
  case GNC_OWNER_CUSTOMER:
  case GNC_OWNER_VENDOR:
    return owner;
  case GNC_OWNER_JOB:
    return gncJobGetOwner (owner->owner.job);
  }
}

int gncOwnerCompare (const GncOwner *a, const GncOwner *b)
{
  if (!a && !b) return 0;
  if (!a && b) return 1;
  if (a && !b) return -1;

  if (a->type != b->type)
    return (a->type - b->type);

  switch (a->type) {
  case GNC_OWNER_NONE:
  case GNC_OWNER_UNDEFINED:
  default:
    return 0;
  case GNC_OWNER_CUSTOMER:
    return gncCustomerCompare (a->owner.customer, b->owner.customer);
  case GNC_OWNER_VENDOR:
    return gncVendorCompare (a->owner.vendor, b->owner.vendor);
  case GNC_OWNER_JOB:
    return gncJobCompare (a->owner.job, b->owner.job);
  }
}

const GUID * gncOwnerGetEndGUID (GncOwner *owner)
{
  if (!owner) return NULL;
  owner = gncOwnerGetEndOwner (owner);
  return gncOwnerGetGUID (owner);
}

gboolean gncOwnerRegister (void)
{
  static QueryObjectDef params[] = {
    { OWNER_TYPE, QUERYCORE_INT64, (QueryAccess)gncOwnerGetType },
    { OWNER_CUSTOMER, GNC_CUSTOMER_MODULE_NAME,
      (QueryAccess)gncOwnerGetCustomer },
    { OWNER_JOB, GNC_JOB_MODULE_NAME, (QueryAccess)gncOwnerGetJob },
    { OWNER_VENDOR, GNC_VENDOR_MODULE_NAME, (QueryAccess)gncOwnerGetVendor },
    { OWNER_PARENT, _GNC_MOD_NAME, (QueryAccess)gncOwnerGetEndOwner },
    { OWNER_PARENTG, QUERYCORE_GUID, (QueryAccess)gncOwnerGetEndGUID },
    { OWNER_NAME, QUERYCORE_STRING, (QueryAccess)gncOwnerGetName },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncOwnerGetGUID },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncOwnerCompare, params);

  return TRUE;
}
