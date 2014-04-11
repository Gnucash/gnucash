/********************************************************************\
 * gncOrder.h -- the Core Business Order Interface                  *
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
/* NOTE: Removed from doxygen by warlord on 2004-05-07 because
 * this module is not fully implemented at this time.
 */
/* @addtogroup Business
    @{ */
/* @addtogroup Order
    @{ */
/* @file gncOrder.h
    @brief Business Order Interface
    @author Copyright (C) 2001 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef GNC_ORDER_H_
#define GNC_ORDER_H_

typedef struct _gncOrder GncOrder;

#include "gncEntry.h"
#include "gncOwner.h"
#include "qof.h"

#define GNC_ID_ORDER "gncOrder"
#define GNC_IS_ORDER(obj)  (QOF_CHECK_TYPE((obj), GNC_ID_ORDER))
#define GNC_ORDER(obj)     (QOF_CHECK_CAST((obj), GNC_ID_ORDER, GncOrder))

/* Create/Destroy Functions */

GncOrder *gncOrderCreate (QofBook *book);
void gncOrderDestroy (GncOrder *order);

/* Set Functions */

void gncOrderSetID (GncOrder *order, const char *id);
void gncOrderSetOwner (GncOrder *order, GncOwner *owner);
void gncOrderSetDateOpened (GncOrder *order, Timespec date);
void gncOrderSetDateClosed (GncOrder *order, Timespec date);
void gncOrderSetNotes (GncOrder *order, const char *notes);
void gncOrderSetReference (GncOrder *order, const char *reference);
void gncOrderSetActive (GncOrder *order, gboolean active);

/* Add an Entry to the Order */
void gncOrderAddEntry (GncOrder *order, GncEntry *entry);
void gncOrderRemoveEntry (GncOrder *order, GncEntry *entry);

/* Get Functions */

const char * gncOrderGetID (GncOrder *order);
GncOwner * gncOrderGetOwner (GncOrder *order);
Timespec gncOrderGetDateOpened (GncOrder *order);
Timespec gncOrderGetDateClosed (GncOrder *order);
const char * gncOrderGetNotes (GncOrder *order);
const char * gncOrderGetReference (GncOrder *order);
gboolean gncOrderGetActive (GncOrder *order);

/* Get the list Entries */
GList * gncOrderGetEntries (GncOrder *order);

void gncOrderBeginEdit (GncOrder *order);
void gncOrderCommitEdit (GncOrder *order);
int gncOrderCompare (GncOrder *a, GncOrder *b);

gboolean gncOrderIsClosed (GncOrder *order);

/** Return a pointer to the instance gncOrder that is identified
 *  by the guid, and is residing in the book. Returns NULL if the 
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncOrder * gncOrderLookup (QofBook *book, const GUID *guid);
 */
#define gncOrderLookup(book,guid)    \
       QOF_BOOK_LOOKUP_ENTITY((book),(guid),GNC_ID_ORDER, GncOrder)

#define ORDER_ID	"id"
#define ORDER_REFERENCE	"reference"
#define ORDER_OWNER	"owner"
#define ORDER_OPENED	"date_opened"
#define ORDER_CLOSED	"date_closed"
#define ORDER_IS_CLOSED	"is_closed?"
#define ORDER_NOTES	"notes"

/** deprecated functions */
#define gncOrderGetGUID(x) qof_instance_get_guid(QOF_INSTANCE(x))
#define gncOrderGetBook(x) qof_instance_get_book(QOF_INSTANCE(x))

#endif /* GNC_ORDER_H_ */
/** @} */
/** @} */
