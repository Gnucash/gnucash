/********************************************************************\
 * qofid.h -- QOF entity identifier API                             *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
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

/** This file defines an API for using the QOF entity identifiers.
 *
 * Identifiers can be used to reference QOF Objects.
 * Identifiers are globally-unique and permanent, i.e., once
 * an entity has been assigned an identifier, it retains that same
 * identifier for its lifetime.
 *
 * Identifiers can be encoded as hex strings. */

#include "guid.h"

/* Identifiers are 'typed' with strings. The ids used in gnucash are
 * defined below. An id with type GNC_ID_NONE does not refer to any
 * entity, although that may change as new ids are created. An id with
 * type GNC_ID_NULL does not refer to any entity, and will never refer
 * to any entity. An identifier with any other type may refer to an
 * actual entity, but that is not guaranteed. If an id does refer to
 * an entity, the type of the entity will match the type of the
 * identifier. */

typedef const char * GNCIdType;
typedef const char * GNCIdTypeConst;

#define QOF_ID_NONE           NULL
#define QOF_ID_BOOK           "Book"
#define QOF_ID_NULL           "null"
#define QOF_ID_SESSION        "Session"


typedef struct gnc_entity_table GNCEntityTable;

GNCIdType xaccGUIDTypeEntityTable (const GUID * guid,
                                   GNCEntityTable *entity_table);

/* Return the type of an identifier.
 * Equivalent function prototype:
 * GNCIdType xaccGUIDType (const GUID * guid, QofBook *book); 
 */

#define xaccGUIDType(guid,book)      \
    xaccGUIDTypeEntityTable ((guid), qof_book_get_entity_table (book))


/* Returns a GUID which is guaranteed to never reference any entity. */
const GUID * xaccGUIDNULL (void);

/* Efficiently allocate & free memory for GUIDs */
GUID * xaccGUIDMalloc (void);
void   xaccGUIDFree (GUID *guid);

/* Callback type for xaccForeachEntity */
typedef void (*foreachObjectCB) (gpointer object, gpointer user_data);

#endif /* QOF_ID_H */
