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

typedef void (*foreachObjectCB) (gpointer object, gpointer user_data);

/* This is the Object Object descriptor */
typedef struct _gncObjectDef {
  gint		version;	/* of the object interface */
  GNCIdType	name;		/* the Object's GNC_ID */
  const char *	type_label;	/* "Printable" type-label string */

  /* book_begin is called from within the Book routines to create
   * module-specific hooks in a book whenever a book is created.
   * book_end is called when the book is being closed, to clean
   * up (and free memory).
   */
  void		(*book_begin)(GNCBook *);
  void		(*book_end)(GNCBook *);

  /* foreach() is used to execute a callback over each object
   * stored in the particular book
   */
  void		(*foreach)(GNCBook *, foreachObjectCB, gpointer);

  /* Given a particular object, return a printable string */
  const char *	(*printable)(gpointer obj);
} GncObject_t;

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


#endif /* GNC_OBJECT_H_ */
