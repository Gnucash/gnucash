/********************************************************************\
 * qofobject.h -- the Core Object Registration/Lookup Interface     *
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
/** @addtogroup Object
    @{ */
/** @addtogroup Objects
    QOF Objects provide the means for associating
    a storage backend to a set of QOF Entities.   While an entity
    can be though of as an identified instance of some thing,  the
    QOF Object provides for a way to associate instances with
    a storage backend.  Storage might be file or SQL storage.

    QOF Objects are also used by the query system ....

    To work with your own QOF Objects, you can use the QOF
    Generator to create sample objects and a mini-application
    with the SQL-type query interface.
    http://qof-gen.sourceforge.net/

    XXX todo, we should split out the storage aspects of this
    thing from the 'foreach' that query depends on.  These are
    kinda unrelated concepts.

    @{ */
/** @file qofobject.h
 *  @brief the Core Object Registration/Lookup Interface
 *  @author Copyright (c) 2001,2002 Derek Atkins <warlord@MIT.EDU>
 */

#ifndef QOF_OBJECT_H_
#define QOF_OBJECT_H_

#include "qofbook.h"
#include "qofid.h"
#include "qofchoice.h"

/** Defines the version of the core object object registration
 * interface.  Only object modules compiled against this version
 * of the interface will load properly
 */
#define QOF_OBJECT_VERSION 3

#define QOF_MOD_OBJECT "qof.object"

typedef struct _QofObject QofObject;
typedef void (*QofForeachCB) (gpointer obj, gpointer user_data);
typedef void (*QofForeachTypeCB) (QofObject *type, gpointer user_data);
typedef void (*QofForeachBackendTypeCB) (QofIdTypeConst type,
        gpointer backend_data,
        gpointer user_data);

/** This is the QofObject Class descriptor
 */
struct _QofObject
{
    gint                interface_version; /* of this object interface */
    QofIdType           e_type;            /* the Object's QOF_ID */
    const char *        type_label;        /* "Printable" type-label string */

    /** Create a new instance of this object type.  This routine might be
     *  NULL if the object type doesn't provide a way of creating new
     *  instances.
     */
    gpointer            (*create)(QofBook *);

    /** book_begin is called from within the Book routines to create
     * module-specific hooks in a book whenever a book is created.
     */
    void                (*book_begin)(QofBook *);

    /** book_end is called when the book is being closed, to clean
     * up (and free memory).
     */
    void                (*book_end)(QofBook *);

    /** Determine if there are any dirty items in this book */
    gboolean            (*is_dirty)(const QofCollection *);

    /** Mark this object's book clean (for after a load) */
    void                (*mark_clean)(QofCollection *);

    /** Traverse over all of the items in the collection, calling
     *  the callback on each item.  The third argument can be any
     *  arbitrary caller-supplied data, and is passed to the callback.
     *  Although (*foreach) may be NULL, allmost all objects should
     *  provide this routine, as without it, little of interest can
      * be done.
     */
    void                (*foreach)(const QofCollection *, QofInstanceForeachCB, gpointer);

    /** Given a particular item of this type, return a printable string.
     */
    const char *        (*printable)(gpointer instance);

    /** Given a pair of items of this type, this routine returns value
     *  indicating which item is 'newer'.  This routine is used by storage
     *  backends to determine if the local or the remote copy of a
     *  particular item is the latest, 'uptodate' version.  Tis routine
     *  should return an integer less than, equal to, or greater than zero
     *  if 'instance_left' is found to be, respecitvely, earlier than, equal
     *  to or later than than 'instance_right'.
     */
    int                 (*version_cmp)(gpointer instance_left, gpointer instance_right);
};

/* -------------------------------------------------------------- */

/** @name Initialize the object registration subsystem */
/** @{ */
void qof_object_initialize (void);
void qof_object_shutdown (void);
/** @} */

/** Register new types of object objects */
gboolean qof_object_register (const QofObject *object);

/** Lookup an object definition */
const QofObject * qof_object_lookup (QofIdTypeConst type_name);

/** Create an instance of the indicated type, returning a pointer to that
 *  instance.  This routine just calls the (*new) callback on the object
 *  definition.
 */
gpointer qof_object_new_instance (QofIdTypeConst type_name, QofBook *book);

/** Get the printable label for a type.  This label is *not*
 * translated; you must use _() on it if you want a translated version.
 */
const char * qof_object_get_type_label (QofIdTypeConst type_name);

/** @return a Human-readable string name for an instance */
const char * qof_object_printable (QofIdTypeConst type_name, gpointer instance);

/** Invoke the callback 'cb' on every object class definition.
 *  The user_data pointer is passed back to the callback.
 */
void qof_object_foreach_type (QofForeachTypeCB cb, gpointer user_data);

/** Invoke the callback 'cb' on every instance ov a particular
 *  object type.  It is presumed that the 'book' stores or somehow
 *  identifies a colllection of instances; thus the callback will
 *  be invoked only for those instances stored in the book.
 */
void qof_object_foreach (QofIdTypeConst type_name, QofBook *book,
                         QofInstanceForeachCB cb, gpointer user_data);

/** Register and lookup backend-specific data for this particular object */
gboolean qof_object_register_backend (QofIdTypeConst type_name,
                                      const char *backend_name,
                                      gpointer be_data);

/*@ dependent @*/
gpointer qof_object_lookup_backend (QofIdTypeConst type_name,
                                    const char *backend_name);

void qof_object_foreach_backend (const char *backend_name,
                                 QofForeachBackendTypeCB cb,
                                 gpointer user_data);

#endif /* QOF_OBJECT_H_ */
/** @} */
/** @} */
