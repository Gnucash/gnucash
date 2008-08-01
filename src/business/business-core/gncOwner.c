/********************************************************************\
 * gncOwner.c -- Business Interface:  Object OWNERs                 *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001, 2002 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>		/* for memcpy() */

#include "gncCustomerP.h"
#include "gncEmployeeP.h"
#include "gncJobP.h"
#include "gncOwner.h"
#include "gncOwnerP.h"
#include "gncVendorP.h"

#define _GNC_MOD_NAME	GNC_ID_OWNER

#define GNC_OWNER_ID	"gncOwner"
#define GNC_OWNER_TYPE	"owner-type"
#define GNC_OWNER_GUID	"owner-guid"

GncOwner * gncOwnerCreate (void)
{
  GncOwner *o;

  o = g_new0 (GncOwner, 1);
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

void gncOwnerInitEmployee (GncOwner *owner, GncEmployee *employee)
{
  if (!owner) return;
  owner->type = GNC_OWNER_EMPLOYEE;
  owner->owner.employee = employee;
}

GncOwnerType gncOwnerGetType (const GncOwner *owner)
{
  if (!owner) return GNC_OWNER_NONE;
  return owner->type;
}

QofIdType
qofOwnerGetType(const GncOwner *owner)
{
	QofIdType type;

	type = NULL;
	switch(owner->type)
	{
		case GNC_OWNER_NONE : {
			type = NULL;
			break;
		}
		case GNC_OWNER_UNDEFINED : {
			type = NULL;
			break;
		}
		case GNC_OWNER_CUSTOMER : {
			type = GNC_ID_CUSTOMER;
			break;
		}
		case GNC_OWNER_JOB : {
			type = GNC_ID_JOB;
			break;
		}
		case GNC_OWNER_VENDOR : {
			type = GNC_ID_VENDOR;
			break;
		}
		case GNC_OWNER_EMPLOYEE : {
			type = GNC_ID_EMPLOYEE;
			break;
		}
	}
	return type;
}

QofInstance*
qofOwnerGetOwner (const GncOwner *owner)
{
	QofInstance *ent;

	if(!owner) { return NULL; }
	ent = NULL;
	switch(owner->type)
	{
		case GNC_OWNER_NONE : {
			break;
		}
		case GNC_OWNER_UNDEFINED : {
			break;
		}
		case GNC_OWNER_CUSTOMER : {
			ent = QOF_INSTANCE(owner->owner.customer);
			break;
		}
		case GNC_OWNER_JOB : {
			ent = QOF_INSTANCE(owner->owner.job);
			break;
		}
		case GNC_OWNER_VENDOR : {
			ent = QOF_INSTANCE(owner->owner.vendor);
			break;
		}
		case GNC_OWNER_EMPLOYEE : {
			ent = QOF_INSTANCE(owner->owner.employee);
			break;
		}
	}
	return ent;
}

void
qofOwnerSetEntity (GncOwner *owner, QofInstance *ent)
{
	if(!owner || !ent) { return; }
	if(0 == safe_strcmp(ent->e_type, GNC_ID_CUSTOMER))
	{
		owner->type = GNC_OWNER_CUSTOMER;
		gncOwnerInitCustomer(owner, (GncCustomer*)ent);
	}
	if(0 == safe_strcmp(ent->e_type, GNC_ID_JOB))
	{
		owner->type = GNC_OWNER_JOB;
		gncOwnerInitJob(owner, (GncJob*)ent);
	}
	if(0 == safe_strcmp(ent->e_type, GNC_ID_VENDOR))
	{
		owner->type = GNC_OWNER_VENDOR;
		gncOwnerInitVendor(owner, (GncVendor*)ent);
	}
	if(0 == safe_strcmp(ent->e_type, GNC_ID_EMPLOYEE))
	{
		owner->type = GNC_OWNER_EMPLOYEE;
		gncOwnerInitEmployee(owner, (GncEmployee*)ent);
	}
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

GncEmployee * gncOwnerGetEmployee (const GncOwner *owner)
{
  if (!owner) return NULL;
  if (owner->type != GNC_OWNER_EMPLOYEE) return NULL;
  return owner->owner.employee;
}

gnc_commodity * gncOwnerGetCurrency (const GncOwner *owner)
{
  if (!owner) return NULL;
  switch (owner->type) {
  case GNC_OWNER_NONE:
  case GNC_OWNER_UNDEFINED:
  default:
    return NULL;
  case GNC_OWNER_CUSTOMER:
    return gncCustomerGetCurrency (owner->owner.customer);
  case GNC_OWNER_VENDOR:
    return gncVendorGetCurrency (owner->owner.vendor);
  case GNC_OWNER_EMPLOYEE:
    return gncEmployeeGetCurrency (owner->owner.employee);
  case GNC_OWNER_JOB:
    return gncOwnerGetCurrency (gncJobGetOwner (owner->owner.job));
  }
}

void gncOwnerCopy (const GncOwner *src, GncOwner *dest)
{
  if (!src || !dest) return;
  if (src == dest) return;
  memcpy (dest, src, sizeof (*dest));
}

GncOwner
gncCloneOwner (const GncOwner *from, QofBook *book)
{
  GncOwner owner = { GNC_OWNER_NONE };
  if (!from) return owner;
  owner.type = from->type;
  switch (from->type)
  {
    case GNC_OWNER_NONE:
      return owner;
    case GNC_OWNER_UNDEFINED:
      owner.owner.undefined = from->owner.undefined;  /* XXX probably wrong ! */
      return owner;
    case GNC_OWNER_CUSTOMER:
      owner.owner.customer = gncCustomerObtainTwin (from->owner.customer, book);
      return owner;
    case GNC_OWNER_JOB:
      owner.owner.job = gncJobObtainTwin (from->owner.job, book);
      return owner;
    case GNC_OWNER_VENDOR:
      owner.owner.vendor = gncVendorObtainTwin (from->owner.vendor, book);
      return owner;
    case GNC_OWNER_EMPLOYEE:
      owner.owner.employee = gncEmployeeObtainTwin (from->owner.employee, book);
      return owner;
    default:
      return owner;
  }
}

gboolean gncOwnerEqual (const GncOwner *a, const GncOwner *b)
{
  if (!a || !b) return FALSE;
  if (gncOwnerGetType (a) != gncOwnerGetType (b)) return FALSE;
  return (a->owner.undefined == b->owner.undefined);
}

const char * gncOwnerGetName (const GncOwner *owner)
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
  case GNC_OWNER_EMPLOYEE:
    return gncAddressGetName(gncEmployeeGetAddr (owner->owner.employee));
  }
}

const GUID * gncOwnerGetGUID (const GncOwner *owner)
{
  if (!owner) return NULL;

  switch (owner->type) {
  case GNC_OWNER_NONE:
  case GNC_OWNER_UNDEFINED:
  default:
    return NULL;
  case GNC_OWNER_CUSTOMER:
    return qof_instance_get_guid (QOF_INSTANCE(owner->owner.customer));
  case GNC_OWNER_JOB:
    return qof_instance_get_guid (QOF_INSTANCE(owner->owner.job));
  case GNC_OWNER_VENDOR:
    return qof_instance_get_guid (QOF_INSTANCE(owner->owner.vendor));
  case GNC_OWNER_EMPLOYEE:
    return qof_instance_get_guid (QOF_INSTANCE(owner->owner.employee));
  }
}

GUID gncOwnerRetGUID (GncOwner *owner)
{
  const GUID *guid = gncOwnerGetGUID (owner);
  if (guid)
    return *guid;
  return *guid_null ();
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
  case GNC_OWNER_EMPLOYEE:
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
  case GNC_OWNER_EMPLOYEE:
    return gncEmployeeCompare (a->owner.employee, b->owner.employee);
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

void gncOwnerAttachToLot (const GncOwner *owner, GNCLot *lot)
{
  KvpFrame *kvp;
  KvpValue *value;
  
  if (!owner || !lot)
    return;

  kvp = gnc_lot_get_slots (lot);

  value = kvp_value_new_gint64 (gncOwnerGetType (owner));
  kvp_frame_set_slot_path (kvp, value, GNC_OWNER_ID, GNC_OWNER_TYPE, NULL);
  kvp_value_delete (value);

  value = kvp_value_new_guid (gncOwnerGetGUID (owner));
  kvp_frame_set_slot_path (kvp, value, GNC_OWNER_ID, GNC_OWNER_GUID, NULL);
  kvp_value_delete (value);

}

gboolean gncOwnerGetOwnerFromLot (GNCLot *lot, GncOwner *owner)
{
  KvpFrame *kvp;
  KvpValue *value;
  GUID *guid;
  QofBook *book;
  GncOwnerType type;

  if (!lot || !owner) return FALSE;

  book = gnc_lot_get_book (lot);
  kvp = gnc_lot_get_slots (lot);

  value = kvp_frame_get_slot_path (kvp, GNC_OWNER_ID, GNC_OWNER_TYPE, NULL);
  if (!value) return FALSE;

  type = kvp_value_get_gint64 (value);

  value = kvp_frame_get_slot_path (kvp, GNC_OWNER_ID, GNC_OWNER_GUID, NULL);
  if (!value) return FALSE;

  guid = kvp_value_get_guid (value);
  if (!guid)
    return FALSE;

  switch (type) {
  case GNC_OWNER_CUSTOMER:
    gncOwnerInitCustomer (owner, gncCustomerLookup (book, guid));
    break;
  case GNC_OWNER_VENDOR:
    gncOwnerInitVendor (owner, gncVendorLookup (book, guid));
    break;
  case GNC_OWNER_EMPLOYEE:
    gncOwnerInitEmployee (owner, gncEmployeeLookup (book, guid));
    break;
  case GNC_OWNER_JOB:
    gncOwnerInitJob (owner, gncJobLookup (book, guid));
    break;
  default:
    return FALSE;
  }
  
  return (owner->owner.undefined != NULL);
}

gboolean gncOwnerIsValid (const GncOwner *owner)
{
  if (!owner) return FALSE;
  return (owner->owner.undefined != NULL);
}

KvpFrame* gncOwnerGetSlots(GncOwner* owner)
{
  if (!owner) return NULL;

  switch (gncOwnerGetType(owner)) {
  case GNC_OWNER_CUSTOMER:
  case GNC_OWNER_VENDOR:
  case GNC_OWNER_EMPLOYEE:
    return qof_instance_get_slots(QOF_INSTANCE(owner->owner.undefined));
  case GNC_OWNER_JOB:
    return gncOwnerGetSlots(gncJobGetOwner(gncOwnerGetJob(owner)));
  default:
    return NULL;
  }
}

/* XXX: Yea, this is broken, but it should work fine for Queries.
 * We're single-threaded, right?
 */
static GncOwner *
owner_from_lot (GNCLot *lot)
{
  static GncOwner owner;

  if (!lot) return NULL;
  if (gncOwnerGetOwnerFromLot (lot, &owner))
    return &owner;

  return NULL;
}

static void
reg_lot (void)
{
  static QofParam params[] = {
    { OWNER_FROM_LOT, _GNC_MOD_NAME, (QofAccessFunc)owner_from_lot, NULL },
    { NULL },
  };

  qof_class_register (GNC_ID_LOT, NULL, params);
}

gboolean gncOwnerGetOwnerFromTypeGuid (QofBook *book, GncOwner *owner, QofIdType type, GUID *guid)
{
  if (!book || !owner || !type || !guid) return FALSE;

  if (0 == safe_strcmp(type, GNC_ID_CUSTOMER)) {
    GncCustomer *customer = gncCustomerLookup(book,guid);
    gncOwnerInitCustomer(owner, customer);
    return (NULL != customer);
  } else if (0 == safe_strcmp(type, GNC_ID_JOB)) {
    GncJob *job = gncJobLookup(book,guid);
    gncOwnerInitJob(owner, job);
    return (NULL != job);
  } else if (0 == safe_strcmp(type, GNC_ID_VENDOR)) {
    GncVendor *vendor = gncVendorLookup(book,guid);
    gncOwnerInitVendor(owner, vendor);
    return (NULL != vendor);
  } else if (0 == safe_strcmp(type, GNC_ID_EMPLOYEE)) {
    GncEmployee *employee = gncEmployeeLookup(book,guid);
    gncOwnerInitEmployee(owner, employee);
    return (NULL != employee);
  }
  return 0;
}

gboolean gncOwnerRegister (void)
{
  static QofParam params[] = {
    { OWNER_TYPE, QOF_TYPE_INT64,      (QofAccessFunc)gncOwnerGetType,          NULL },
    { OWNER_CUSTOMER, GNC_ID_CUSTOMER, (QofAccessFunc)gncOwnerGetCustomer,      NULL },
    { OWNER_JOB, GNC_ID_JOB,           (QofAccessFunc)gncOwnerGetJob,           NULL },
    { OWNER_VENDOR, GNC_ID_VENDOR,     (QofAccessFunc)gncOwnerGetVendor,        NULL },
    { OWNER_EMPLOYEE, GNC_ID_EMPLOYEE, (QofAccessFunc)gncOwnerGetEmployee,      NULL },
    { OWNER_PARENT, GNC_ID_OWNER,      (QofAccessFunc)gncOwnerGetEndOwner,      NULL },
    { OWNER_PARENTG, QOF_TYPE_GUID,    (QofAccessFunc)gncOwnerGetEndGUID,       NULL },
    { OWNER_NAME, QOF_TYPE_STRING,     (QofAccessFunc)gncOwnerGetName, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID,   (QofAccessFunc)gncOwnerGetGUID, NULL },
    { NULL },
  };

  qof_class_register (GNC_ID_OWNER, (QofSortFunc)gncOwnerCompare, params);
  reg_lot ();

  return TRUE;
}
