/*
 * gncObject.h -- the Core Object Registration/Lookup Interface
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_OBJECT_H_
#define GNC_OBJECT_H_

#include "gnc-book.h"
#include "GNCId.h"

/* Defines the version of the core object object registration
 * interface.  Only object modules compiled against this version
 * of the interface will load properly
 */
#define GNC_OBJECT_VERSION 1

typedef struct _gncObjectDef GncObject_t;
typedef void (*foreachObjectCB) (gpointer object, gpointer user_data);
typedef void (*foreachTypeCB) (GncObject_t *type, gpointer user_data);
typedef void (*foreachBackendTypeCB) (GNCIdTypeConst type,
				      gpointer backend_data,
				      gpointer user_data);

/* This is the Object Object descriptor */
struct _gncObjectDef {
  gint		interface_version;	/* of this object interface */
  GNCIdType	name;		/* the Object's GNC_ID */
  const char *	type_label;	/* "Printable" type-label string */

  /* book_begin is called from within the Book routines to create
   * module-specific hooks in a book whenever a book is created.
   * book_end is called when the book is being closed, to clean
   * up (and free memory).
   */
  void		(*book_begin)(GNCBook *);
  void		(*book_end)(GNCBook *);

  /* Determine if there are any dirty items in this book */
  gboolean	(*is_dirty)(GNCBook *);

  /* Mark this object's book clean (for after a load) */
  void		(*mark_clean)(GNCBook *);

  /* foreach() is used to execute a callback over each object
   * stored in the particular book
   */
  void		(*foreach)(GNCBook *, foreachObjectCB, gpointer);

  /* Given a particular object, return a printable string */
  const char *	(*printable)(gpointer obj);

};

void gncObjectForeachType (foreachTypeCB cb, gpointer user_data);

void gncObjectForeach (GNCIdTypeConst type_name, GNCBook *book, 
		       foreachObjectCB cb, gpointer user_data);

const char * gncObjectPrintable (GNCIdTypeConst type_name, gpointer obj);


/* REGISTRATION AND REG-LOOKUP FUNCTIONS */

/* Register new types of object objects */
gboolean gncObjectRegister (const GncObject_t *object);

/* Get the printable label for a type */
const char * gncObjectGetTypeLabel (GNCIdTypeConst type_name);

/* Lookup a object definition */
const GncObject_t * gncObjectLookup (GNCIdTypeConst type_name);


/* Register and lookup backend-specific data for this particular object */
gboolean gncObjectRegisterBackend (GNCIdTypeConst type_name,
				   const char *backend_name,
				   gpointer be_data);

gpointer gncObjectLookupBackend (GNCIdTypeConst type_name,
				 const char *backend_name);

void gncObjectForeachBackend (const char *backend_name,
			      foreachBackendTypeCB cb,
			      gpointer user_data);

#endif /* GNC_OBJECT_H_ */
