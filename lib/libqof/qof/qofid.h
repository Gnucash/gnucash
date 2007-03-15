/********************************************************************\
 * qofid.h -- QOF entity type identification system                 *
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

#ifndef QOF_ID_H
#define QOF_ID_H 

/** @addtogroup Entity 
    @{ */
/** @addtogroup Entities

    This file defines an API that adds types to the GUID's.
    GUID's with types can be used to identify and reference 
    typed entities.  

    The idea here is that a GUID can be used to uniquely identify
    some thing.  By adding a type, one can then talk about the 
    type of thing identified.  By adding a collection, one can
    then work with a handle to a collection of things of a given
    type, each uniquely identified by a given ID.  QOF Entities
    can be used independently of any other part of the system.
    In particular, Entities can be useful even if one is not using
    the Query ond Object parts of the QOF system.
  
    Identifiers are globally-unique and permanent, i.e., once
    an entity has been assigned an identifier, it retains that same
    identifier for its lifetime.
    Identifiers can be encoded as hex strings. 
   
    GUID Identifiers are 'typed' with strings.  The native ids used 
    by QOF are defined below.
	-# An id with type QOF_ID_NONE does not 
    refer to any entity.
	-# An id with type QOF_ID_NULL does not refer
	to any entity, and will never refer to any entity.
	=# An identifier with any other type may refer to an
    actual entity, but that is not guaranteed as that entity does
	not have to exist within the current book. (See ::PARTIAL_QOFBOOK).
	Also, creating a new entity from a data source involves creating
	a temporary GUID and then setting the value from the data source.
	If an id does refer to an entity, the type of the entity will match
	the type of the identifier. 

    If you have a type name, and you want to have a way of finding
    a collection that is associated with that type, then you must use
    Books.

	Entities can refer to other entities as well as to the basic
	QOF types, using the qofclass parameters.

 @{ */
/** @file qofid.h
    @brief QOF entity type identification system 
    @author Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu> 
    @author Copyright (C) 2003 Linas Vepstas <linas@linas.org>
    @author Copyright (C) 2007 Daniel Espinosa <esodan@gmail.com>
*/
#include "qofinstance.h"
#include <string.h>
#include <glib-object.h>
#include "guid.h"


/******************* QofCollection ****************/


/** QofLogModule declaration */
typedef const gchar* QofLogModule;

/** QofIdType declaration */
typedef GType QofIdType;
/** QofIdTypeConst declaration */
typedef GType QofIdTypeConst;

#define QOF_ID_NONE           G_TYPE_NONE
#define QOF_ID_NULL           G_TYPE_INVALID
#define QOF_ID_BOOK           QOF_TYPE_BOOK
#define QOF_ID_SESSION        "Session"


/* GObject declarations */

#define QOF_TYPE_COLLECTION            (qof_collection_get_type ())
#define QOF_COLLECTION(o)              (G_TYPE_CHECK_INSTANCE_CAST ((o), QOF_TYPE_COLLECTION, QofCollection))
#define QOF_COLLECTION_CLASS(k)        (G_TYPE_CHECK_CLASS_CAST((k), QOF_TYPE_COLLECTION, QofCollectionClass))
#define QOF_IS_COLLECTION(o)           (G_TYPE_CHECK_INSTANCE_TYPE ((o), QOF_TYPE_COLLECTION))
#define QOF_IS_COLLECTION_CLASS(k)     (G_TYPE_CHECK_CLASS_TYPE ((k), QOF_TYPE_COLLECTION))
#define QOF_COLLECTION_GET_CLASS(o)    (G_TYPE_INSTANCE_GET_CLASS ((o), QOF_TYPE_COLLECTION, QofCollectionClass))

typedef struct _QofCollectionClass QofCollectionClass;
typedef struct _QofCollectionPrivate QofCollectionPrivate;
typedef struct _QofCollection QofCollection;

struct _QofCollection
{
	/* GObject Parent */
   GObject parent;
   QofCollectionPrivate *priv;
};


struct _QofCollectionClass {
	GObjectClass parent_class;
	/* virtual table */

	/* Add Signal Functions Here */
};


GType qof_collection_get_type (void);

/** create a new collection of entities of type */
QofCollection * qof_collection_new (GType type);

/** return the number of entities in the collection. */
guint qof_collection_count (const QofCollection *col);

void qof_collection_destroy (QofCollection *col);

/** return the type that the collection stores */
GType qof_collection_get_g_type (const QofCollection *col);


/** Find the entity going only from its guid */
QofInstance* qof_collection_lookup_element (const QofCollection *col, const GUID *guid);

/** Call the callback for each entity in the collection. */
void qof_collection_foreach (QofCollection *col, QofInstanceForeachCB cb_func, 
                             gpointer user_data);

/** Store and retreive arbitrary object-defined data 
 *
 * XXX We need to add a callback for when the collection is being
 * destroyed, so that the user has a chance to clean up anything
 * that was put in the 'data' member here.
 */
#define qof_collection_get_data(col) (g_object_get_data(G_OBJECT(col), "SOME_DATA"))
#define qof_collection_set_data(col, user_data) (g_object_set_data(G_OBJECT(col), "SOME_DATA", user_data))

/** Return value of 'dirty' flag on collection */
gboolean qof_collection_is_dirty (const QofCollection *col);

/** @name QOF_TYPE_COLLECT: Linking one entity to many of one type

\note These are \b NOT the same as the main collections in the book.

QOF_TYPE_COLLECT is a secondary collection, used to select entities
of one object type as references of another entity.
\sa QOF_TYPE_CHOICE.

@{
*/
/** \brief Add an entity to a QOF_TYPE_COLLECT.

\note These are \b NOT the same as the main collections in the book.

Entities can be
freely added and merged across these secondary collections, they
will not be removed from the original collection as they would
by using ::qof_instanceinsert_entity or ::qof_instanceremove_entity. 

*/
gboolean qof_collection_add_element (QofCollection *coll, QofInstance *inst);

gboolean qof_collection_remove_element (QofCollection *coll, QofInstance *inst);


/* \brief Merge two QOF_TYPE_COLLECT of the same type.

\note \b NOT the same as the main collections in the book.

QOF_TYPE_COLLECT uses a secondary collection, independent of
those in the book. Entities will not be removed from the
original collection as when using ::qof_instance_insert_entity
or ::qof_instanceremove_entity.

*/
gboolean
qof_collection_merge (QofCollection *target, QofCollection *merge);

/** \brief Compare two secondary collections.

Performs a deep comparision of the collections. Each QofEntity in
each collection is looked up in the other collection, via the GUID.

\return 0 if the collections are identical or both are NULL
otherwise -1 if target is NULL or either collection contains an entity with an invalid
GUID or if the types of the two collections do not match,
or +1 if merge is NULL or if any entity exists in one collection but
not in the other.
*/
gint
qof_collection_compare (QofCollection *target, QofCollection *merge);

/** \brief Create a secondary collection from a GList

@param type The QofIdType of the QofCollection \b and of 
	\b all entities in the GList.
@param glist GList of entities of the same QofIdType.

@return NULL if any of the entities fail to match the
	QofCollection type, else a pointer to the collection
	on success.
*/
QofCollection*
qof_collection_from_glist (QofBook *book, GType      type, GList *glist);

/******************************************************/

/** Is QOF operating in "alternate" dirty mode.  In normal mode,
 *  whenever an instance is dirtied, the collection (and therefore the
 *  book) is immediately marked as dirty.  In alternate mode, the
 *  collection is only marked dirty when a dirty instance is
 *  committed.  If a dirty instance is freed instead of committed, the
 *  dirty state of collection (and therefore the book) is never
 *  changed. */
gboolean qof_get_alt_dirty_mode (void);

/** Set QOF into "alternate" dirty mode.  In normal mode, whenever an
 *  instance is dirtied, the collection (and therefore the book) is
 *  immediately marked as dirty.  In alternate mode, the collection is
 *  only marked dirty when a dirty instance is committed.  If a dirty
 *  instance is freed instead of committed, the dirty state of
 *  collection (and therefore the book) is never changed. */
void qof_set_alt_dirty_mode (gboolean enabled);


/** @} */
/** @} */

#endif /* QOF_ID_H */
/** @} */
/** @} */
