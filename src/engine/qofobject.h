/********************************************************************\
 * qofobject.h -- the Core Object Registration/Lookup Interface     *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/** @addtogroup Engine
    @{ */
/** @file qofobject.h
 * @breif the Core Object Registration/Lookup Interface
 *
 * @author Copyright (c) 2001,2002, Derek Atkins <warlord@MIT.EDU>
 */

#ifndef QOF_OBJECT_H_
#define QOF_OBJECT_H_

#include "qofbook.h"

/* Defines the version of the core object object registration
 * interface.  Only object modules compiled against this version
 * of the interface will load properly
 */
#define GNC_OBJECT_VERSION 1

typedef struct _QofObject QofObject;
typedef void (*QofForeachTypeCB) (QofObject *type, gpointer user_data);
typedef void (*QofForeachBackendTypeCB) (QofIdTypeConst type,
				      gpointer backend_data,
				      gpointer user_data);

/* This is the Object Object descriptor */
struct _QofObject {
  gint		interface_version;	/* of this object interface */
  QofIdType	name;		/* the Object's GNC_ID */
  const char *	type_label;	/* "Printable" type-label string */

  /* book_begin is called from within the Book routines to create
   * module-specific hooks in a book whenever a book is created.
   * book_end is called when the book is being closed, to clean
   * up (and free memory).
   */
  void		(*book_begin)(QofBook *);
  void		(*book_end)(QofBook *);

  /* Determine if there are any dirty items in this book */
  gboolean	(*is_dirty)(QofBook *);

  /* Mark this object's book clean (for after a load) */
  void		(*mark_clean)(QofBook *);

  /* foreach() is used to execute a callback over each object
   * stored in the particular book
   */
  void		(*foreach)(QofBook *, QofEntityForeachCB, gpointer);

  /* Given a particular object, return a printable string */
  const char *	(*printable)(gpointer obj);

};

void qof_object_foreach_type (QofForeachTypeCB cb, gpointer user_data);

void qof_object_foreach (QofIdTypeConst type_name, QofBook *book, 
		       QofEntityForeachCB cb, gpointer user_data);

const char * qof_object_printable (QofIdTypeConst type_name, gpointer obj);


/* REGISTRATION AND REG-LOOKUP FUNCTIONS */

/** Register new types of object objects */
gboolean qof_object_register (const QofObject *object);

/** Get the printable label for a type.  This label is *not*
 * translated; you must use _() on it if you want a translated version.
 */
const char * qof_object_get_type_label (QofIdTypeConst type_name);

/** Lookup a object definition */
const QofObject * qof_object_lookup (QofIdTypeConst type_name);


/** Register and lookup backend-specific data for this particular object */
gboolean qof_object_register_backend (QofIdTypeConst type_name,
				   const char *backend_name,
				   gpointer be_data);

gpointer qof_object_lookup_backend (QofIdTypeConst type_name,
				 const char *backend_name);

void qof_object_foreach_backend (const char *backend_name,
			      QofForeachBackendTypeCB cb,
			      gpointer user_data);

#endif /* QOF_OBJECT_H_ */
/** @} */
