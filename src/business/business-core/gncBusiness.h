/*
 * gncBusiness.h -- the Core Business Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BUSINESS_H_
#define GNC_BUSINESS_H_

#include "gnc-session.h"

/* Defines the version of the core business object registration
 * interface.  Only business modules compiled against this version
 * of the interface will load properly
 */
#define GNC_BUSINESS_VERSION 1

typedef struct _gncBusinessObject GncBusinessObject;
typedef struct _gncBusiness GncBusiness;

/* This is the Business Object descriptor */
struct _gncBusinessObject {
  gint		version;
  const char *	name;
  const char *	type_label;
  void		(*destroy)(GncBusiness *);
  GList *	(*get_list)(GncBusiness *, gboolean show_all);
  const char *	(*printable)(gpointer obj);
};

/* Create and Destroy the Gnc Business subsystem state */
GncBusiness *gncBusinessCreate (GNCSession *session);
void gncBusinessDestroy (GncBusiness *business);

/* Return the GNC Session from the Business Object */
GNCSession * gncBusinessGetSession (const GncBusiness *bus);

/* Obtain an object from the type and GUID */
gpointer gncBusinessLookupGUID (GncBusiness *business, const char *type_name,
				const GUID * guid);

GList * gncBusinessGetList (GncBusiness *business, const char *type_name,
			    gboolean show_all);

const char * gncBusinessPrintable (GncBusiness *business,
				   const char *type_name,
				   gpointer obj);

/* Grab the entity table for an object */
GHashTable * gncBusinessEntityTable (GncBusiness *business, const char *name);

/* Add an entity to the table */
void gncBusinessAddEntity (GncBusiness *business, const char *name,
			   const GUID *guid, gpointer obj);

/* Remove it from the table */
void gncBusinessRemoveEntity (GncBusiness *business, const char *name,
			      const GUID *guid);

/* REGISTRATION AND REG-LOOKUP FUNCTIONS */

/* Register new types of business objects */
gboolean gncBusinessRegister (const GncBusinessObject *object);

/* Get the printable label for a type */
const char * gncBusinessGetTypeLabel (const char *type_name);

/* Lookup a business object */
const GncBusinessObject * gncBusinessLookup (const char *name);


#endif /* GNC_BUSINESS_H_ */
