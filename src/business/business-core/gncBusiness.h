/*
 * gncBusiness.h -- the Core Business Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BUSINESS_H_
#define GNC_BUSINESS_H_

#include "gnc-book.h"

/* Defines the version of the core business object registration
 * interface.  Only business modules compiled against this version
 * of the interface will load properly
 */
#define GNC_BUSINESS_VERSION 1

typedef struct _gncBusinessObject GncBusinessObject;

/* This is the Business Object descriptor */
struct _gncBusinessObject {
  gint		version;
  const char *	name;
  const char *	type_label;
  void		(*create)(GNCBook *);
  void		(*destroy)(GNCBook *);
  GList *	(*get_list)(GNCBook *, gboolean show_all);
  const char *	(*printable)(gpointer obj);
};

void gncBusinessCreateBook (GNCBook *book);

void gncBusinessDestroyBook (GNCBook *book);

GList * gncBusinessGetList (GNCBook *book, const char *type_name,
			    gboolean show_all);

const char * gncBusinessPrintable (const char *type_name, gpointer obj);


/* REGISTRATION AND REG-LOOKUP FUNCTIONS */

/* Register new types of business objects */
gboolean gncBusinessRegister (const GncBusinessObject *object);

/* Get the printable label for a type */
const char * gncBusinessGetTypeLabel (const char *type_name);

/* Lookup a business object */
const GncBusinessObject * gncBusinessLookup (const char *name);


#endif /* GNC_BUSINESS_H_ */
