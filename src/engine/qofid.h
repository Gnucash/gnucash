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
 *  GUID's with types cna be used to identify and reference 
 * typed entities.
 *
 * GUID Identifiers can be used to reference QOF Objects.
 * Identifiers are globally-unique and permanent, i.e., once
 * an entity has been assigned an identifier, it retains that same
 * identifier for its lifetime.
 *
 * Identifiers can be encoded as hex strings. 
 */

#include "guid.h"

/** GUID Identifiers are 'typed' with strings. The native ids used 
 * by QOF are defined below. An id with type QOF_ID_NONE does not 
 * refer to any entity, although that may change (???). An id with 
 * type QOF_ID_NULL does not refer to any entity, and will never refer
 * to any entity. An identifier with any other type may refer to an
 * actual entity, but that is not guaranteed (??? Huh?).  If an id 
 * does refer to an entity, the type of the entity will match the 
 * type of the identifier. 
 */

typedef const char * QofIdType;
typedef const char * QofIdTypeConst;

#define QOF_ID_NONE           NULL
#define QOF_ID_BOOK           "Book"
#define QOF_ID_NULL           "null"
#define QOF_ID_SESSION        "Session"


typedef struct _QofEntityTable QofEntityTable;

/** Return the type of the indicated guid */
QofIdType qof_entity_type (QofEntityTable *entity_table, const GUID * guid);

/* Callback type for qof_entity_foreach */
typedef void (*QofEntityForeachCB) (gpointer object, gpointer user_data);

#endif /* QOF_ID_H */
/** @} */

