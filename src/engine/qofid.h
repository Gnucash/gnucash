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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef QOF_ID_H
#define QOF_ID_H 

/** @addtogroup Engine
    @{ */
/** @file qofid.h
    @brief QOF entity type identification system 
    @author Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu> 
    @author Copyright (C) 2003 Linas Vepstas <linas@linas.org>
*/

/** This file defines an API that adds types to the GUID's.
 *  GUID's with types can be used to identify and reference 
 *  typed entities.
 *
 * GUID Identifiers can be used to reference QOF Objects.
 * Identifiers are globally-unique and permanent, i.e., once
 * an entity has been assigned an identifier, it retains that same
 * identifier for its lifetime.
 *
 * Identifiers can be encoded as hex strings. 
 *
 * GUID Identifiers are 'typed' with strings.  The native ids used 
 * by QOF are defined below. An id with type QOF_ID_NONE does not 
 * refer to any entity, although that may change (???). An id with 
 * type QOF_ID_NULL does not refer to any entity, and will never refer
 * to any entity. An identifier with any other type may refer to an
 * actual entity, but that is not guaranteed (??? Huh?).  If an id 
 * does refer to an entity, the type of the entity will match the 
 * type of the identifier. 
 */

#include <string.h>
#include "guid.h"

typedef const char * QofIdType;
typedef const char * QofIdTypeConst;

#define QOF_ID_NONE           NULL
#define QOF_ID_BOOK           "Book"
#define QOF_ID_NULL           "null"
#define QOF_ID_SESSION        "Session"

/* simple,cheesy cast but holds water for now */
#define QOF_ENTITY(object) ((QofEntity *)(object))

/* Inline string comparision; compiler will optimize away most of this */
#define QSTRCMP(da,db) ({                \
  int val = 0;                           \
  if ((da) && (db)) {                    \
    if ((da) != (db)) {                  \
      val = strcmp ((da), (db));         \
    }                                    \
  } else                                 \
  if ((!(da)) && (db)) {                 \
    val = -1;                            \
  } else                                 \
  if ((da) && (!(db))) {                 \
    val = 1;                             \
  }                                      \
  val; /* block assumes value of last statment */  \
})

/** return TRUE if object is of the given type */
#define QOF_CHECK_TYPE(obj,type) (0 == QSTRCMP((type),(((QofEntity *)(obj))->e_type)))

/** cast object to the indicated type, print error message if its bad  */
#define QOF_CHECK_CAST(obj,e_type,c_type) (                   \
  QOF_CHECK_TYPE((obj),(e_type)) ?                            \
  (c_type *) (obj) :                                          \
  (c_type *) ({                                               \
     g_log (G_LOG_DOMAIN, G_LOG_LEVEL_CRITICAL,               \
       "Error: Bad QofEntity at %s:%d", __FILE__, __LINE__);  \
     (obj);                                                   \
  }))


typedef struct QofEntity_s QofEntity;
typedef struct QofCollection_s QofCollection;

struct QofEntity_s
{
   QofIdType        e_type;
	GUID             guid;
	QofCollection  * collection;
};

/** Initialise the memory associated with an entity */
void qof_entity_init (QofEntity *, QofIdType, QofCollection *);
                                                                                
/** Release the data associated with this entity. Dont actually free
 * the memory associated with the instance. */
void qof_entity_release (QofEntity *);

/* Return the GUID of this entity */
const GUID * qof_entity_get_guid (QofEntity *);

/** collections of entities */
QofCollection * qof_collection_new (QofIdType type);
void qof_collection_destroy (QofCollection *col);

/** return the type that the collection stores */
QofIdType qof_collection_get_type (QofCollection *);

/** Find the entity going only from its guid */
QofEntity * qof_collection_lookup_entity (QofCollection *, const GUID *);

/** Callback type for qof_entity_foreach */
typedef void (*QofEntityForeachCB) (QofEntity *, gpointer user_data);

/** Call the callback for each entity in the collection. */
void qof_collection_foreach (QofCollection *, 
                       QofEntityForeachCB, gpointer user_data);

/** store and retreive arbitrary object-defined data */
gpointer qof_collection_get_data (QofCollection *col);
void qof_collection_set_data (QofCollection *col, gpointer user_data);

/* Return value of 'dirty' flag on collection */
gboolean qof_collection_is_dirty (QofCollection *col);


#endif /* QOF_ID_H */
/** @} */

