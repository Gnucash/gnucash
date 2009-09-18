/***************************************************************************
 *            qofreference.h
 *
 *  Mon Feb 13 21:07:06 2006
 *  Copyright  2006  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef _QOFREFERENCE_H
#define _QOFREFERENCE_H

/** \addtogroup Reference

Partial book is a QofBook that lacks at least one of the key elements of a fully
structured (complete) book:
	- Self-contained: All relationships between entities are satisfied within
		the book itself;
	- Containing specific entities that provide an external structure to the
		data within the book.

Partial books are useful for query results, selective export and data mining
but need to be merged back into standard books. It is not supported to construct
a partial book and then convert the same book to a standard book.

Different backends have different requirements for a complete book - some
(like gnucash) are highly customised to that application - however all complete
QofBooks must be self-contained, only a partial book uses QofInstanceReference.

To retain the relationships between entities, including between a partial and
a complete book, QofInstanceReference data is stored in the QofBook. This data
should be read by backends that support partial books so that the exported
data contains the GUID and QofIdType of the referenced entity. Even if that
entity does not then exist within the partial book, it can be located when
the partial book is merged back into the original, complete, book. (Remember
that given the GUID and QofIdType of any QofInstance it is possible to uniquely
identify that entity in another book.)

Entities in partial books may need to refer to the entities that remain within
the partial book. Once all the entities you want are in the partial book,
call qof_book_set_references to restore as many references as possible. Each
object type is checked in turn, each entity of that type and then each
parameter that can relate to another entity. Any references that cannot be
found are left unset - depending on the object these may be undefined or NULL.
(It is advisable to set all QOF parameters to either a default value or NULL
in the create: routine for the object but QOF has no way of guaranteeing this.)

@{
*/

/** \file qofreference.h
	\brief Dealing with relationships between entities in partial books.
	\author Copyright (c) 2006 Neil Williams <linux@codehelp.co.uk>
*/

#include "qof.h"

/** @name Using a partial QofBook.

Part of the handling for partial books requires a storage mechanism for
references to entities that are not within reach of the partial book.
This requires a GList in the book data to contain the reference
QofIdType and GUID so that when the book is written out, the
reference can be included. See ::qof_book_get_data.

When the file is imported back in, the list needs to be rebuilt.
The QSF backend rebuilds the references by linking to real entities.
Other backends can process the list in similar ways.

The list stores the QofInstanceReference to the referenced entity -
a struct that contains the GUID and the QofIdType of the referenced
entity as well as the parameter used to obtain the reference.

Partial books need to be differentiated in the backend, the
flag in the book data is used by qof_session_save to prevent a partial
book being saved using a backend that requires a full book. Forcing this
flag would cause data loss so always merge a partial book with the complete
book (even if that book is initially empty) before trying to save the data
using a backend that does not support partial books.

@{ */


/** \brief External references in a partial QofBook.

For use by any session that deals with partial QofBooks.
It is used by the entity copy functions and by the QSF backend.
Creates a GList stored in the Book hashtable to contain
repeated references for a single entity.
*/
typedef struct qof_instance_reference
{
    QofIdType       choice_type;/**< Used when the reference is a QOF_TYPE_CHOICE type
	- stores the actual type of the reference from the list of available choices. */
    QofIdType       type;       /**< The type of the original entity -
	use the param->param_type to obtain the type of the reference entity.
	For a QOF_TYPE_COLLECT, obtain the collection and get the type from that. */
    GUID            *ref_guid;  /**< The GUID of the REFERENCE entity */
    const QofParam  *param;      /**< The parameter of the original entity to use
	to get or set the reference. */
    const GUID      *ent_guid;   /**< The GUID of the original entity. */
} QofInstanceReference;

/** \brief Adds a new reference to the partial book data hash.

Retrieves any existing reference list and appends the new reference.

If the book is not already marked as partial, it will be marked as
partial.
*/
void
qof_session_update_reference_list(QofSession *session, QofInstanceReference *reference);

/** Used as the key value for the QofBook data hash.
 *
 * Retrieved later by QSF (or any other suitable backend) to
 * rebuild the references from the QofInstanceReference struct
 * that contains the QofIdType and GUID of the referenced entity
 * of the original QofBook as well as the parameter data and the
 * GUID of the original entity.
 * */
#define ENTITYREFERENCE "QofInstanceReference"

/** \brief Flag indicating a partial QofBook.

When set in the book data with a gboolean value of TRUE,
the flag denotes that only a backend that supports partial
books can be used to save this session.
*/

#define PARTIAL_QOFBOOK "PartialQofBook"

/** \brief Read QofInstanceReference data for this book and set values.

@param book The partial book containing the referenceList

The referenceList is a GList of QofInstanceReference structures that contain
the GUID of each end of a reference. e.g. where one entity refers to another.

The referenceList is used in partial books to store relationships between
entities when the entities themselves might not exist in the partial book.

If the book is not marked as a partial book, an assertion error is generated.

This routine tries to lookup each entity in the referenceList for the
book and then tries to lookup the reference - to find the child entity that
was originally linked to this parent. The child entity is then set in the
parent so that it can be located as normal.

If the child entity does not exist in this partial book, the parent entity
is not updated. The referenceList is unchanged (in case the child is added
later).

*/
void qof_book_set_references(QofBook *book);

/** \brief Get a reference from this entity to another entity.

\note Only to be used in situations where the QofParam has already
been checked \b NOT to be QOF_TYPE_COLLECT or other known QOF types
because this function expects to return a single reference and
a collect parameter would need to return a list of references, other
parameters would not return a viable QofInstance. (A string cannot be
cast to an entity.)

Used in the preparation of a partial QofBook when the known entity
(the one currently being copied into the partial book) refers to
any other entity, usually as a parent or child.
The routine calls the param_getfcn of the supplied parameter,
which must return an object (QofInstance*), not a known QOF data type, to
retrieve the referenced entity and therefore the GUID. The GUID of
both entities are stored in the reference which then needs to be added
to the reference list which is added to the partial book data hash.
The reference itself is used to preserve the relationship
between entities within and outside the partial book.

See also ::qof_class_get_referenceList to obtain the list of
parameters that provide references to the known entity whilst
excluding parameters that return known QOF data types.

Note that even if the referenced entity \b exists in the partial
book (or will exist later), a reference must still be obtained and
added to the reference list for the book itself. This maintains
the integrity of the partial book during sequential copy operations.

@param ent   The known entity.
@param param  The parameter to use to get the referenced entity.

@return FALSE on error, otherwise a pointer to the QofInstanceReference.
*/
QofInstanceReference*
qof_instance_get_reference_from(QofInstance *ent, const QofParam *param);

/** @} */
/** @} */
#endif /* _QOFREFERENCE_H */
